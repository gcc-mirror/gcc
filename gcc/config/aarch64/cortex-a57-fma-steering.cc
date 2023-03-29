/* FMA steering optimization pass for Cortex-A57.
   Copyright (C) 2015-2023 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_LIST
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "df.h"
#include "insn-config.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfganal.h"
#include "insn-attr.h"
#include "context.h"
#include "tree-pass.h"
#include "function-abi.h"
#include "regrename.h"
#include "aarch64-protos.h"

/* For better performance, the destination of FMADD/FMSUB instructions should
   have the same parity as their accumulator register if the accumulator
   contains the result of a previous FMUL or FMADD/FMSUB instruction if
   targetting Cortex-A57 processors.  Performance is also increased by
   otherwise keeping a good balance in the parity of the destination register
   of FMUL or FMADD/FMSUB.

   This pass ensure that registers are renamed so that these conditions hold.
   We reuse the existing register renaming facility from regrename.cc to build
   dependency chains and expose candidate registers for renaming.


   The algorithm has three steps:

   First, the functions of the register renaming pass are called.  These
   analyze the instructions and produce a list of def/use chains of
   instructions.

   Next, this information is used to build trees of multiply and
   multiply-accumulate instructions.  The roots of these trees are any
   multiply, or any multiply-accumulate whose accumulator is not dependent on
   a multiply or multiply-accumulate instruction.  A child is added to the
   tree where a dependency chain exists between the result of the parent
   instruction and the accumulator operand of the child, as in the diagram
   below:

		 fmul s2, s0, s1
		/		\
   fmadd s0, s1, s1, s2   fmadd s4, s1, s1 s2
	    |
   fmadd s3, s1, s1, s0

   Trees made of a single instruction are permitted.

   Finally, renaming is performed.  The parity of the destination register at
   the root of a tree is checked against the current balance of multiply and
   multiply-accumulate on each pipeline.  If necessary, the root of a tree is
   renamed, in which case the rest of the tree is then renamed to keep the same
   parity in the destination registers of all instructions in the tree.  */



/* Forward declarations.  */
class fma_node;
class fma_root_node;
class func_fma_steering;

/* Dependencies between FMUL or FMADD/FMSUB instructions and subsequent
   FMADD/FMSUB instructions form a graph.  This is because alternatives can
   make a register be set by several FMUL or FMADD/FMSUB instructions in
   different basic blocks and because of loops.  For ease of browsing, the
   connected components of this graph are broken up into forests of trees.
   Forests are represented by fma_forest objects, contained in the fma_forests
   list.  Using a separate object for the forests allows for a better use of
   memory as there is some information that is global to each forest, such as
   the number of FMSUB and FMADD/FMSUB instructions currently scheduled on each
   floating-point execution pipelines.  */

class fma_forest
{
public:
  fma_forest (func_fma_steering *, fma_root_node *, int);
  ~fma_forest ();

  int get_id ();
  std::list<fma_root_node *> *get_roots ();
  func_fma_steering *get_globals ();
  int get_target_parity ();
  void fma_node_created (fma_node *);
  void merge_forest (fma_forest *);
  void dump_info ();
  void dispatch ();

private:
  /* Prohibit copy construction.  */
  fma_forest (const fma_forest &);

  /* The list of roots that form this forest.  */
  std::list<fma_root_node *> *m_roots;

  /* Target parity the destination register of all FMUL and FMADD/FMSUB
     instructions in this forest should have.  */
  int m_target_parity;

  /* Link to the instance of func_fma_steering holding data related to the
     FMA steering of the current function (cfun).  */
  func_fma_steering *m_globals;

  /* Identifier for the forest (used for dumps).  */
  int m_id;

  /* Total number of nodes in the forest (for statistics).  */
  int m_nb_nodes;
};

class fma_node
{
public:
  fma_node (fma_node *parent, du_chain *chain);
  ~fma_node ();

  bool root_p ();
  fma_forest *get_forest ();
  std::list<fma_node *> *get_children ();
  rtx_insn *get_insn ();
  void add_child (fma_node *);
  int get_parity ();
  void set_head (du_head *);
  void rename (fma_forest *);
  void dump_info (fma_forest *);

private:
  /* Prohibit copy construction.  */
  fma_node (const fma_node &);

protected:
  /* Root node that lead to this node.  */
  fma_root_node *m_root;

  /* The parent node of this node.  If the node belong to a chain with several
     parent nodes, the first one encountered in a depth-first search is chosen
     as canonical parent.  */
  fma_node *m_parent;

  /* The list of child nodes.  If a chain contains several parent nodes, one is
     chosen as canonical parent and the others will have no children.  */
  std::list<fma_node *> *m_children;

  /* The associated DU_HEAD chain that the insn represented by this object
     is (one of) the root of.  When a chain contains several roots, the non
     canonical ones have this field set to NULL.  */
  struct du_head *m_head;

  /* The FMUL or FMADD/FMSUB instruction this object corresponds to.  */
  rtx_insn *m_insn;
};

class fma_root_node : public fma_node
{
public:
  fma_root_node (func_fma_steering *, du_chain *, int);

  fma_forest *get_forest ();
  void set_forest (fma_forest *);
  void dump_info (fma_forest *);

private:
  /* The forest this node belonged to when it was created.  */
  fma_forest *m_forest;
};

/* Class holding all data and methods relative to the FMA steering of a given
   function.  The FMA steering pass could then run in parallel for different
   functions.  */

class func_fma_steering
{
public:
  func_fma_steering ();
  ~func_fma_steering ();

  int get_fpu_balance ();
  void remove_forest (fma_forest *);
  bool put_node (fma_node *);
  void update_balance (int);
  fma_node *get_fma_node (rtx_insn *);
  void analyze_fma_fmul_insn (fma_forest *, du_chain *, du_head_p);
  void execute_fma_steering ();

private:
  /* Prohibit copy construction.  */
  func_fma_steering (const func_fma_steering &);

  void dfs (void (*) (fma_forest *), void (*) (fma_forest *, fma_root_node *),
	    void (*) (fma_forest *, fma_node *), bool);
  void analyze ();
  void rename_fma_trees ();

  /* Mapping between FMUL or FMADD/FMSUB instructions and the associated
     fma_node object.  Used when analyzing an instruction that is a root of
     a chain to find if such an object was created because this instruction
     is also a use in another chain.  */
  hash_map<rtx_insn *, fma_node *> *m_insn_fma_head_map;

  /* A list of all the forests in a given function.  */
  std::list<fma_forest *> m_fma_forests;

  /* Balance of FMUL and FMADD/FMSUB instructions between the two FPU
     pipelines:
     < 0: more instruction dispatched to the first pipeline
     == 0: perfect balance
     > 0: more instruction dispatched to the second pipeline.  */
  int m_fpu_balance;

  /* Identifier for the next forest created.  */
  int m_next_forest_id;
};

/* Rename the register HEAD->regno in all the insns in the chain HEAD to any
   register not in the set UNAVAILABLE.  Adapted from rename_chains in
   regrename.cc.  */

static bool
rename_single_chain (du_head_p head, HARD_REG_SET *unavailable)
{
  int best_new_reg;
  int n_uses = 0;
  struct du_chain *tmp;
  int reg = head->regno;
  enum reg_class super_class = NO_REGS;

  if (head->cannot_rename)
    return false;

  if (fixed_regs[reg] || global_regs[reg]
      || (frame_pointer_needed && reg == HARD_FRAME_POINTER_REGNUM))
    return false;

  /* Iterate over elements in the chain in order to:
     1. Count number of uses, and narrow the set of registers we can
	use for renaming.
     2. Compute the superunion of register classes in this chain.  */
  for (tmp = head->first; tmp; tmp = tmp->next_use)
    {
      if (DEBUG_INSN_P (tmp->insn))
	continue;
      n_uses++;
      *unavailable |= ~reg_class_contents[tmp->cl];
      super_class = reg_class_superunion[(int) super_class][(int) tmp->cl];
    }

  if (n_uses < 1)
    return false;

  best_new_reg = find_rename_reg (head, super_class, unavailable, reg,
				  false);

  if (dump_file)
    {
      fprintf (dump_file, "Register %s in insn %d", reg_names[reg],
	       INSN_UID (head->first->insn));
      if (head->call_abis)
	fprintf (dump_file, " crosses a call");
    }

  if (best_new_reg == reg)
    {
      if (dump_file)
	fprintf (dump_file, "; no available better choice\n");
      return false;
    }

  if (regrename_do_replace (head, best_new_reg))
    {
      if (dump_file)
	fprintf (dump_file, ", renamed as %s\n", reg_names[best_new_reg]);
      df_set_regs_ever_live (best_new_reg, true);
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, ", renaming as %s failed\n",
		 reg_names[best_new_reg]);
      return false;
    }
  return true;
}

/* Return whether T is the attribute of a FMADD/FMSUB-like instruction.  */

static bool
is_fmac_op (enum attr_type t)
{
  return (t == TYPE_FMACS) || (t == TYPE_FMACD) || (t == TYPE_NEON_FP_MLA_S);
}

/* Return whether T is the attribute of a FMUL instruction.  */

static bool
is_fmul_op (enum attr_type t)
{
  return (t == TYPE_FMULS) || (t == TYPE_FMULD) || (t == TYPE_NEON_FP_MUL_S);
}

/* Return whether INSN is an FMUL (if FMUL_OK is true) or FMADD/FMSUB
   instruction.  */

static bool
is_fmul_fmac_insn (rtx_insn *insn, bool fmul_ok)
{
  enum attr_type t;

  if (!NONDEBUG_INSN_P (insn))
    return false;

  if (recog_memoized (insn) < 0)
    return false;

  /* Only consider chain(s) this instruction is a root of if this is an FMUL or
     FMADD/FMSUB instruction.  This allows to avoid browsing chains of all
     instructions for FMUL or FMADD/FMSUB in them.  */
  t = get_attr_type (insn);
  return is_fmac_op (t) || (fmul_ok && is_fmul_op (t));
}


/*
 * Class fma_forest method definitions.
 */

fma_forest::fma_forest (func_fma_steering *fma_steer, fma_root_node *fma_root,
			int id)
{
      memset (this, 0, sizeof (*this));
      this->m_globals = fma_steer;
      this->m_roots = new std::list<fma_root_node *>;
      this->m_roots->push_back (fma_root);
      this->m_id = id;
}

fma_forest::~fma_forest ()
{
  delete this->m_roots;
}

int
fma_forest::get_id ()
{
  return this->m_id;
}

std::list<fma_root_node *> *
fma_forest::get_roots ()
{
  return this->m_roots;
}

func_fma_steering *
fma_forest::get_globals ()
{
  return this->m_globals;
}

int
fma_forest::get_target_parity ()
{
  return this->m_target_parity;
}

/* Act on the creation of NODE by updating statistics in FOREST and adding an
   entry for it in the func_fma_steering hashmap.  */

void fma_forest::fma_node_created (fma_node *node)
{
  bool created = !this->m_globals->put_node (node);

  gcc_assert (created);
  this->m_nb_nodes++;
}

/* Merge REF_FOREST and OTHER_FOREST together, making REF_FOREST the canonical
   fma_forest object to represent both.  */

void
fma_forest::merge_forest (fma_forest *other_forest)
{
  std::list<fma_root_node *> *other_roots;
  std::list<fma_root_node *>::iterator other_root_iter;

  if (this == other_forest)
    return;

  other_roots = other_forest->m_roots;

  /* Update root nodes' pointer to forest.  */
  for (other_root_iter = other_roots->begin ();
       other_root_iter != other_roots->end (); ++other_root_iter)
    (*other_root_iter)->set_forest (this);

  /* Remove other_forest from the list of forests and move its tree roots in
     the list of tree roots of ref_forest.  */
  this->m_globals->remove_forest (other_forest);
  this->m_roots->splice (this->m_roots->begin (), *other_roots);
  this->m_nb_nodes += other_forest->m_nb_nodes;

  delete other_forest;
}

/* Dump information about the forest FOREST.  */

void
fma_forest::dump_info ()
{
  gcc_assert (dump_file);

  fprintf (dump_file, "Forest #%d has %d nodes\n", this->m_id,
	   this->m_nb_nodes);
}

/* Wrapper around fma_forest::dump_info for use as parameter of function
   pointer type in func_fma_steering::dfs.  */

static void
dump_forest_info (fma_forest *forest)
{
  forest->dump_info ();
}

/* Dispatch forest to the least utilized pipeline.  */

void
fma_forest::dispatch ()
{
  this->m_target_parity = this->m_roots->front ()->get_parity ();
  int fpu_balance = this->m_globals->get_fpu_balance ();
  if (fpu_balance != 0)
    this->m_target_parity = (fpu_balance < 0);

  if (dump_file)
    fprintf (dump_file, "Target parity for forest #%d: %s\n", this->m_id,
	     this->m_target_parity ? "odd" : "even");
}

/* Wrapper around fma_forest::dispatch for use as parameter of function pointer
   type in func_fma_steering::dfs.  */

static void
dispatch_forest (fma_forest *forest)
{
  forest->dispatch ();
}

fma_node::fma_node (fma_node *parent, du_chain *chain)
{
  memset (this, 0, sizeof (*this));
  this->m_parent = parent;
  this->m_children = new std::list<fma_node *>;
  this->m_insn = chain->insn;
  /* root_p () cannot be used to check for root before root is set.  */
  if (this->m_parent == this)
    this->m_root = static_cast<fma_root_node *> (parent);
  else
    {
      this->m_root = parent->m_root;
      this->get_forest ()->fma_node_created (this);
    }
}

fma_node::~fma_node ()
{
  delete this->m_children;
}

std::list<fma_node *> *
fma_node::get_children ()
{
  return this->m_children;
}

rtx_insn *
fma_node::get_insn ()
{
  return this->m_insn;
}

void
fma_node::set_head (du_head *head)
{
  gcc_assert (!this->m_head);
  this->m_head = head;
}

/* Add a child to this node in the list of children.  */

void
fma_node::add_child (fma_node *child)
{
  this->m_children->push_back (child);
}

/* Return the parity of the destination register of the instruction represented
   by this node.  */

int
fma_node::get_parity ()
{
  return this->m_head->regno % 2;
}

/* Get the actual forest associated with a non root node as the one the node
   points to might have been merged into another one.  In that case the pointer
   in the root nodes are updated so we return the forest pointer of a root node
   pointed to by the initial forest.  Despite being a oneliner, this method is
   defined here as it references a method from fma_root_node.  */

fma_forest *
fma_node::get_forest ()
{
  return this->m_root->get_forest ();
}

/* Return whether a node is a root node.  */

bool
fma_node::root_p ()
{
  return this->m_root == this;
}

/* Dump information about the children of node FMA_NODE in forest FOREST.  */

void
fma_node::dump_info (ATTRIBUTE_UNUSED fma_forest *forest)
{
  struct du_chain *chain;
  std::list<fma_node *>::iterator fma_child;

  gcc_assert (dump_file);

  if (this->get_children ()->empty ())
    return;

  fprintf (dump_file, "Instruction(s)");
  for (chain = this->m_head->first; chain; chain = chain->next_use)
    {
      if (!is_fmul_fmac_insn (chain->insn, true))
	continue;

      if (chain->loc != &SET_DEST (PATTERN (chain->insn)))
	continue;

      fprintf (dump_file, " %d", INSN_UID (chain->insn));
    }

  fprintf (dump_file, " is(are) accumulator dependency of instructions");
  for (fma_child = this->get_children ()->begin ();
       fma_child != this->get_children ()->end (); fma_child++)
    fprintf (dump_file, " %d", INSN_UID ((*fma_child)->m_insn));
  fprintf (dump_file, "\n");
}

/* Wrapper around fma_node::dump_info for use as parameter of function pointer
   type in func_fma_steering::dfs.  */

static void
dump_tree_node_info (fma_forest *forest, fma_node *node)
{
  node->dump_info (forest);
}

/* Rename the destination register of a single FMUL or FMADD/FMSUB instruction
   represented by FMA_NODE to a register that respect the target parity for
   FOREST or with same parity of the instruction represented by its parent node
   if it has one.  */

void
fma_node::rename (fma_forest *forest)
{
  int cur_parity, target_parity;

  /* This is alternate root of a chain and thus has no children.  It will be
     renamed when processing the canonical root for that chain.  */
  if (!this->m_head)
    return;

  target_parity = forest->get_target_parity ();
  if (this->m_parent)
    target_parity = this->m_parent->get_parity ();
  cur_parity = this->get_parity ();

  /* Rename if parity differs.  */
  if (cur_parity != target_parity)
    {
      rtx_insn *insn = this->m_insn;
      HARD_REG_SET unavailable;
      machine_mode mode;
      int reg;

      if (dump_file)
	{
	  unsigned cur_dest_reg = this->m_head->regno;

	  fprintf (dump_file, "FMA or FMUL at insn %d but destination "
		   "register (%s) has different parity from expected to "
		   "maximize FPU pipeline utilization\n", INSN_UID (insn),
		   reg_names[cur_dest_reg]);
	}

      /* Don't clobber traceback for noreturn functions.  */
      CLEAR_HARD_REG_SET (unavailable);
      if (frame_pointer_needed)
	{
	  add_to_hard_reg_set (&unavailable, Pmode, FRAME_POINTER_REGNUM);
	  add_to_hard_reg_set (&unavailable, Pmode, HARD_FRAME_POINTER_REGNUM);
	}

      /* Exclude registers with wrong parity.  */
      mode = GET_MODE (SET_DEST (PATTERN (insn)));
      for (reg = cur_parity; reg < FIRST_PSEUDO_REGISTER; reg += 2)
	add_to_hard_reg_set (&unavailable, mode, reg);

      if (!rename_single_chain (this->m_head, &unavailable))
	{
	  if (dump_file)
	    fprintf (dump_file, "Destination register of insn %d could not be "
		     "renamed. Dependent FMA insns will use this parity from "
		     "there on.\n", INSN_UID (insn));
	}
      else
	cur_parity = target_parity;
    }

  forest->get_globals ()->update_balance (cur_parity);
}

/* Wrapper around fma_node::dump_info for use as parameter of function pointer
   type in func_fma_steering::dfs.  */

static void
rename_fma_node (fma_forest *forest, fma_node *node)
{
  node->rename (forest);
}

fma_root_node::fma_root_node (func_fma_steering *globals, du_chain *chain,
			      int id) : fma_node (this, chain)
{
  this->m_forest = new fma_forest (globals, this, id);
  this->m_forest->fma_node_created (this);
}

fma_forest *
fma_root_node::get_forest ()
{
  return this->m_forest;
}

void
fma_root_node::set_forest (fma_forest *ref_forest)
{
  this->m_forest = ref_forest;
}

/* Dump information about the roots of forest FOREST.  */

void
fma_root_node::dump_info (fma_forest *forest)
{
  gcc_assert (dump_file);

  if (this == forest->get_roots ()->front ())
    fprintf (dump_file, "Instruction(s) at root of forest #%d:",
	     forest->get_id ());
  fprintf (dump_file, " %d", INSN_UID (this->m_insn));
  if (this == forest->get_roots ()->back ())
    fprintf (dump_file, "\n");
}

/* Wrapper around fma_root_node::dump_info for use as parameter of function
   pointer type in func_fma_steering::dfs.  */

static void
dump_tree_root_info (fma_forest *forest, fma_root_node *node)
{
  node->dump_info (forest);
}

func_fma_steering::func_fma_steering () : m_fpu_balance (0)
{
  this->m_insn_fma_head_map = new hash_map<rtx_insn *, fma_node *>;
  this->m_fma_forests.clear ();
  this->m_next_forest_id = 0;
}

func_fma_steering::~func_fma_steering ()
{
  delete this->m_insn_fma_head_map;
}

int
func_fma_steering::get_fpu_balance ()
{
  return this->m_fpu_balance;
}

void
func_fma_steering::remove_forest (fma_forest *forest)
{
  this->m_fma_forests.remove (forest);
}

/* Memorize the mapping of this instruction to its fma_node object and return
   whether such a mapping existed.  */

bool
func_fma_steering::put_node (fma_node *node)
{
  return this->m_insn_fma_head_map->put (node->get_insn (), node);
}

/* Update the current balance considering a node with the given PARITY.  */

void
func_fma_steering::update_balance (int parity)
{
  this->m_fpu_balance = parity ? this->m_fpu_balance + 1
			       : this->m_fpu_balance - 1;
}

/* Return whether an fma_node object exists for instruction INSN and, if not,
   allocate one in *RET.  */

fma_node *
func_fma_steering::get_fma_node (rtx_insn *insn)
{
  fma_node **fma_slot;

  fma_slot = this->m_insn_fma_head_map->get (insn);
  if (fma_slot)
    return *fma_slot;
  return NULL;
}

/* Allocate and initialize fma_node objects for the FMUL or FMADD/FMSUB
   instruction in CHAIN->insn and its dependent FMADD/FMSUB instructions, all
   part of FOREST.  For the children, the associated head is left untouched
   (and thus null) as this function will be called again when considering the
   chain where they are def.  For the parent, the chain is given in HEAD.  */

void
func_fma_steering::analyze_fma_fmul_insn (fma_forest *ref_forest,
					  du_chain *chain, du_head_p head)
{
  fma_forest *forest;
  fma_node *node = this->get_fma_node (chain->insn);

  /* This is a root node.  */
  if (!node)
    {
      fma_root_node *root_node;

      root_node = new fma_root_node (this, chain, this->m_next_forest_id++);
      forest = root_node->get_forest ();
      node = root_node;

      /* Until proved otherwise, assume this root is not part of an existing
	 forest and thus add its forest to the list of forests.  */
      this->m_fma_forests.push_back (forest);
    }
  else
    forest = node->get_forest ();

  node->set_head (head);

  /* fma_node is part of a chain with several defs, one of them having already
     been processed.  The root of that already processed def is the canonical
     one and the root of fma_node is added to its forest.  No need to process
     the children nodes as they were already processed when the other def was
     processed.  */
  if (ref_forest)
    {
      ref_forest->merge_forest (forest);
      return;
    }

  for (chain = head->first; chain; chain = chain->next_use)
    {
      fma_node *child_fma;
      rtx fma_rtx, *accum_rtx_p;

      if (!is_fmul_fmac_insn (chain->insn, false))
	continue;

      /* Get FMA rtx.  */
      fma_rtx = SET_SRC (PATTERN (chain->insn));
      /* FMA is negated.  */
      if (GET_CODE (fma_rtx) == NEG)
	fma_rtx = XEXP (fma_rtx, 0);
      /* Get accumulator rtx.  */
      accum_rtx_p = &XEXP (fma_rtx, 2);
      /* Accumulator is negated.  */
      if (!REG_P (*accum_rtx_p))
	accum_rtx_p = &XEXP (*accum_rtx_p, 0);

      /* This du_chain structure is not for the accumulator register.  */
      if (accum_rtx_p != chain->loc)
	continue;

      /* If object already created, this is a loop carried dependency.  We
	 don't include this object in the children as we want trees for
	 rename_fma_trees to not be an infinite loop.  */
      if (this->get_fma_node (chain->insn))
	continue;

      child_fma = new fma_node (node, chain);

      /* Memorize the mapping of this instruction to its fma_node object
	 as it will be processed for the chain starting at its destination
	 register later.  */

      /* Link to siblings.  */
      node->add_child (child_fma);
    }
}

/* Perform a depth-first search of the forests of fma_node in
   THIS->m_fma_forests, calling PROCESS_FOREST () on each fma_forest object in
   THIS->m_fma_forests list, PROCESS_ROOT () on each tree root and
   PROCESS_NODE () on each node.  If FREE is true, free all std::list in the
   same dfs.  */

void
func_fma_steering::dfs (void (*process_forest) (fma_forest *),
			void (*process_root) (fma_forest *, fma_root_node *),
			void (*process_node) (fma_forest *, fma_node *),
			bool free)
{
  auto_vec<fma_node *> to_process;
  auto_vec<fma_node *> to_free;
  std::list<fma_forest *>::iterator forest_iter;

  /* For each forest.  */
  for (forest_iter = this->m_fma_forests.begin ();
       forest_iter != this->m_fma_forests.end (); ++forest_iter)
    {
      std::list<fma_root_node *>::iterator root_iter;

      if (process_forest)
	process_forest (*forest_iter);

      /* For each tree root in this forest.  */
      for (root_iter = (*forest_iter)->get_roots ()->begin ();
	   root_iter != (*forest_iter)->get_roots ()->end (); ++root_iter)
	{
	  if (process_root)
	    process_root (*forest_iter, *root_iter);
	  to_process.safe_push (*root_iter);
	}

      /* For each tree node in this forest.  */
      while (!to_process.is_empty ())
	{
	  fma_node *node;
	  std::list<fma_node *>::iterator child_iter;

	  node = to_process.pop ();

	  if (process_node)
	    process_node (*forest_iter, node);

	  for (child_iter = node->get_children ()->begin ();
	       child_iter != node->get_children ()->end (); ++child_iter)
	    to_process.safe_push (*child_iter);

	  /* Defer freeing so that the process_node callback can access the
	     parent and children of the node being processed.  */
	  if (free)
	    to_free.safe_push (node);
	}

      if (free)
	{
	  delete *forest_iter;

	  while (!to_free.is_empty ())
	    {
	      fma_node *node = to_free.pop ();
	      if (node->root_p ())
		delete static_cast<fma_root_node *> (node);
	      else
		delete node;
	    }
	}
    }
}

/* Build the dependency trees of FMUL and FMADD/FMSUB instructions.  */

void
func_fma_steering::analyze ()
{
  int i, n_blocks, *bb_dfs_preorder;
  basic_block bb;
  rtx_insn *insn;

  bb_dfs_preorder = XNEWVEC (int, last_basic_block_for_fn (cfun));
  n_blocks = pre_and_rev_post_order_compute (bb_dfs_preorder, NULL, false);

  /* Browse the graph of basic blocks looking for FMUL or FMADD/FMSUB
     instructions.  */
  for (i = 0; i < n_blocks; i++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, bb_dfs_preorder[i]);
      FOR_BB_INSNS (bb, insn)
	{
	  operand_rr_info *dest_op_info;
	  struct du_chain *chain = NULL;
	  unsigned dest_regno;
	  fma_forest *forest = NULL;
	  du_head_p head = NULL;
	  int i;

	  if (!is_fmul_fmac_insn (insn, true))
	    continue;

	  /* Search the chain where this instruction is (one of) the root.  */
	  dest_op_info = insn_rr[INSN_UID (insn)].op_info;
	  dest_regno = REGNO (SET_DEST (PATTERN (insn)));
	  for (i = 0; i < dest_op_info->n_chains; i++)
	    {
	      /* The register tracked by this chain does not match the
		 destination register of insn.  */
	      if (dest_op_info->heads[i]->regno != dest_regno)
		continue;

	      head = dest_op_info->heads[i];
	      /* The chain was merged in another, find the new head.  */
	      if (!head->first)
		head = regrename_chain_from_id (head->id);

	      /* Search the chain element for this instruction and, if another
		 FMUL or FMADD/FMSUB instruction was already processed, note
		 the forest of its tree.  */
	      forest = NULL;
	      for (chain = head->first; chain; chain = chain->next_use)
		{
		  fma_node **fma_slot;

		  if (!is_fmul_fmac_insn (chain->insn, true))
		    continue;

		  /* This is a use, continue.  */
		  if (chain->loc != &SET_DEST (PATTERN (chain->insn)))
		    continue;

		  if (chain->insn == insn)
		    break;

		  fma_slot = this->m_insn_fma_head_map->get (chain->insn);
		  if (fma_slot && (*fma_slot)->get_children ())
		    forest = (*fma_slot)->get_forest ();
		}
	      if (chain)
		break;
	    }

	  /* Due to implementation of regrename, dest register can slip away
	     from regrename's analysis.  As a result, there is no chain for
	     the destination register of insn.  We simply skip the insn even
	     it is a fmul/fmac instruction.  This can happen when the dest
	     register is also a source register of insn and one of the below
	     conditions is satisfied:
	       1) the source reg is setup in larger mode than this insn;
	       2) the source reg is uninitialized;
	       3) the source reg is passed in as parameter.  */
	  if (i < dest_op_info->n_chains)
	    this->analyze_fma_fmul_insn (forest, chain, head);
	}
    }
  free (bb_dfs_preorder);

  if (dump_file)
    this->dfs (dump_forest_info, dump_tree_root_info, dump_tree_node_info,
	       false);
}

/* Perform the renaming of all chains with FMUL or FMADD/FMSUB involved with
   the objective of keeping FPU pipeline balanced in term of instructions and
   having FMADD/FMSUB with dependencies on previous FMUL or FMADD/FMSUB be
   scheduled on the same pipeline.  */

void
func_fma_steering::rename_fma_trees ()
{
  this->dfs (dispatch_forest, NULL, rename_fma_node, true);

  if (dump_file && !this->m_fma_forests.empty ())
    {
      fprintf (dump_file, "Function %s has ", current_function_name ());
      if (this->m_fpu_balance == 0)
	fprintf (dump_file, "perfect balance of FMUL/FMA chains between the "
		 "two FPU pipelines\n");
      else if (this->m_fpu_balance > 0)
	fprintf (dump_file, "%d more FMUL/FMA chains scheduled on the second "
		 "FPU pipeline\n", this->m_fpu_balance);
      else /* this->m_fpu_balance < 0 */
	fprintf (dump_file, "%d more FMUL/FMA chains scheduled on the first "
		 "FPU pipeline\n", - this->m_fpu_balance);
    }
}

/* Execute FMA steering pass.  */

void
func_fma_steering::execute_fma_steering ()
{
  df_set_flags (DF_LR_RUN_DCE);
  df_note_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  regrename_init (true);
  regrename_analyze (NULL);
  this->analyze ();
  this->rename_fma_trees ();
  regrename_finish ();
}

const pass_data pass_data_fma_steering =
{
  RTL_PASS, /* type */
  "fma_steering", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_fma_steering : public rtl_opt_pass
{
public:
  pass_fma_steering (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_fma_steering, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (aarch64_tune_params.extra_tuning_flags
	      & AARCH64_EXTRA_TUNE_RENAME_FMA_REGS)
	      && optimize >= 2;
    }

  virtual unsigned int execute (function *)
    {
      func_fma_steering *fma_steering = new func_fma_steering;
      fma_steering->execute_fma_steering ();
      delete fma_steering;
      return 0;
    }

}; // class pass_fma_steering

/* Create a new fma steering pass instance.  */

rtl_opt_pass *
make_pass_fma_steering (gcc::context *ctxt)
{
  return new pass_fma_steering (ctxt);
}
