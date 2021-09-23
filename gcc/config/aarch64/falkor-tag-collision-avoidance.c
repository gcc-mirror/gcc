/* Tag Collision Avoidance pass for Falkor.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.

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
#include "tree.h"
#include "tree-pass.h"
#include "aarch64-protos.h"
#include "hash-map.h"
#include "cfgloop.h"
#include "cfgrtl.h"
#include "rtl-iter.h"
#include "df.h"
#include "memmodel.h"
#include "optabs.h"
#include "regs.h"
#include "recog.h"
#include "function-abi.h"
#include "regrename.h"
#include "print-rtl.h"

/* The Falkor hardware prefetching system uses the encoding of the registers
   and offsets of loads to decide which of the multiple hardware prefetchers to
   assign the load to.  This has the positive effect of accelerating prefetches
   when all related loads with uniform strides are assigned to the same
   prefetcher unit.  The down side is that because of the way the assignment
   works, multiple unrelated loads may end up on the same prefetch unit, thus
   causing the unit to bounce between different sets of addresses and never
   train correctly.  The point of this pass is to avoid such collisions so that
   unrelated loads are spread out to different prefetchers.  It also makes a
   rudimentary attempt to ensure that related loads with the same tags don't
   get moved out unnecessarily.

   Perhaps a future enhancement would be to make a more concerted attempt to
   get related loads under the same tag.  See the memcpy/memset implementation
   for falkor in glibc to understand the kind of impact this can have on
   falkor.

   The assignment of loads is based on a tag that is computed from the encoding
   of the first destination register (the only destination in case of LDR), the
   base register and the offset (either the register or the immediate value, as
   encoded in the instruction).  This is what the 14 bit tag looks like:

   |<- 6 bits ->|<- 4b ->|<- 4b ->|
   --------------------------------
   |  OFFSET    |  SRC   |  DST   |
   --------------------------------

   For all cases, the SRC and DST are the 4 LSB of the encoding of the register
   in the instruction.  Offset computation is more involved and is as follows:

   - For register offset addressing: 4 LSB of the offset register with the MSB
     of the 6 bits set to 1.

   - For immediate offset: 4 LSB of the encoded immediate offset.  The encoding
     depends on the width of the load and is expressed as multiples of the
     width.

   - For loads with update: 4 LSB of the offset.  The encoding here is the
     exact number by which the base is offset and incremented.

   Based on the above it is clear that registers 0 and 16 will result in
   collisions, 1 and 17 and so on.  This pass detects such collisions within a
   def/use chain of the source register in a loop and tries to resolve the
   collision by renaming one of the destination registers.  */

/* Get the destination part of the tag.  */
#define TAG_GET_DEST(__tag) ((__tag) & 0xf)

/* Get the tag with the destination part updated.  */
#define TAG_UPDATE_DEST(__tag, __dest) (((__tag) & ~0xf) | (__dest & 0xf))

#define MAX_PREFETCH_STRIDE 2048

/* The instruction information structure.  This is used to cache information
   about the INSN that we derive when traversing through all of the insns in
   loops.  */
class tag_insn_info
{
public:
  rtx_insn *insn;
  rtx dest;
  rtx base;
  rtx offset;
  bool writeback;
  bool ldp;

  tag_insn_info (rtx_insn *i, rtx d, rtx b, rtx o, bool w, bool p)
    : insn (i), dest (d), base (b), offset (o), writeback (w), ldp (p)
  {}

  /* Compute the tag based on BASE, DEST and OFFSET of the load.  */
  unsigned tag ()
    {
      unsigned int_offset = 0;
      rtx offset = this->offset;
      unsigned dest = REGNO (this->dest);
      unsigned base = REGNO (this->base);
      machine_mode dest_mode = GET_MODE (this->dest);

      /* Falkor does not support SVE; GET_LOAD_INFO ensures that the
	 destination mode is constant here.  */
      unsigned dest_mode_size = GET_MODE_SIZE (dest_mode).to_constant ();

      /* For loads of larger than 16 bytes, the DEST part of the tag is 0.  */
      if ((dest_mode_size << this->ldp) > 16)
	dest = 0;

      if (offset && REG_P (offset))
	int_offset = (1 << 5) | REGNO (offset);
      else if (offset && CONST_INT_P (offset))
	{
	  int_offset = INTVAL (offset);
	  int_offset /= dest_mode_size;
	  if (!this->writeback)
	    int_offset >>= 2;
	}
      return ((dest & 0xf)
	      | ((base & 0xf) << 4)
	      | ((int_offset & 0x3f) << 8));
    }
};

/* Hash map to traverse and process instructions with colliding tags.  */
typedef hash_map <rtx, auto_vec <tag_insn_info *> > tag_map_t;

/* Vector of instructions with colliding tags.  */
typedef auto_vec <tag_insn_info *> insn_info_list_t;

/* Pair of instruction information and unavailable register set to pass to
   CHECK_COLLIDING_TAGS.  */
typedef std::pair <tag_insn_info *, HARD_REG_SET *> arg_pair_t;


/* Callback to free all tag_insn_info objects.  */
bool
free_insn_info (const rtx &t ATTRIBUTE_UNUSED, insn_info_list_t *v,
		void *arg ATTRIBUTE_UNUSED)
{
  while (v->length () > 0)
    delete v->pop ();

  return true;
}


/* Add all aliases of the register to the unavailable register set.  REG is the
   smallest register number that can then be used to reference its aliases.
   UNAVAILABLE is the hard register set to add the ignored register numbers to
   and MODE is the mode in which the registers would have been used.  */
static void
ignore_all_aliases (HARD_REG_SET *unavailable, machine_mode mode, unsigned reg)
{
  add_to_hard_reg_set (unavailable, mode, reg);
  add_to_hard_reg_set (unavailable, mode, reg + 16);
  add_to_hard_reg_set (unavailable, mode, reg + 32);
  add_to_hard_reg_set (unavailable, mode, reg + 48);
}


/* Callback to check which destination registers are unavailable to us for
   renaming because of the base and offset colliding.  This is a callback that
   gets called for every name value pair (T, V) in the TAG_MAP.  The ARG is an
   std::pair of the tag_insn_info of the original insn and the hard register
   set UNAVAILABLE that is used to record hard register numbers that cannot be
   used for the renaming.  This always returns true since we want to traverse
   through the entire TAG_MAP.  */
bool
check_colliding_tags (const rtx &t, const insn_info_list_t &v, arg_pair_t *arg)
{
  HARD_REG_SET *unavailable = arg->second;
  unsigned orig_tag = arg->first->tag ();
  unsigned tag = INTVAL (t);
  machine_mode mode = GET_MODE (arg->first->dest);

  /* Can't collide with emptiness.  */
  if (v.length () == 0)
    return true;

  /* Drop all aliased destination registers that result in the same
     tag.  It is not necessary to drop all of them but we do anyway
     because it is quicker than checking ranges.  */
  if (TAG_UPDATE_DEST (tag, 0) == TAG_UPDATE_DEST (orig_tag, 0))
    ignore_all_aliases (unavailable, mode, TAG_GET_DEST (tag));

  return true;
}


/* Initialize and build a set of hard register numbers UNAVAILABLE to avoid for
   renaming.  INSN_INFO is the original insn, TAG_MAP is the map of the list of
   insns indexed by their tags, HEAD is the def/use chain head of the
   destination register of the original insn.  The routine returns the super
   class of register classes that may be used during the renaming.  */
static enum reg_class
init_unavailable (tag_insn_info *insn_info, tag_map_t &tag_map, du_head_p head,
		  HARD_REG_SET *unavailable)
{
  unsigned dest = head->regno;
  enum reg_class super_class = NO_REGS;
  machine_mode mode = GET_MODE (insn_info->dest);

  CLEAR_HARD_REG_SET (*unavailable);

  for (struct du_chain *tmp = head->first; tmp; tmp = tmp->next_use)
    {
      if (DEBUG_INSN_P (tmp->insn))
	continue;

      *unavailable |= ~reg_class_contents[tmp->cl];
      super_class = reg_class_superunion[(int) super_class][(int) tmp->cl];
    }

  for (unsigned i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (fixed_regs[i] || global_regs[i])
      add_to_hard_reg_set (unavailable, mode, i);

  arg_pair_t arg = arg_pair_t (insn_info, unavailable);

  /* Exclude all registers that would lead to collisions with other loads.  */
  tag_map.traverse <arg_pair_t *, check_colliding_tags> (&arg);

  /* Finally, also ignore all aliases of the current reg.  */
  ignore_all_aliases (unavailable, mode, dest & 0xf);

  return super_class;
}


/* Find a suitable and available register and rename the chain of occurrences
   of the register  defined in the def/use chain headed by HEAD in which INSN
   exists.  CUR_TAG, TAGS and TAG_MAP are used to determine which registers are
   unavailable due to a potential collision due to the rename.  The routine
   returns the register number in case of a successful rename or -1 to indicate
   failure.  */
static int
rename_chain (tag_insn_info *insn_info, tag_map_t &tag_map, du_head_p head)
{
  unsigned dest_regno = head->regno;

  if (head->cannot_rename || head->renamed)
    return -1;

  HARD_REG_SET unavailable;

  enum reg_class super_class = init_unavailable (insn_info, tag_map, head,
						 &unavailable);

  unsigned new_regno = find_rename_reg (head, super_class, &unavailable,
					dest_regno, false);

  /* Attempt to rename as long as regrename doesn't just throw the same
     register at us.  */
  if (new_regno != dest_regno && regrename_do_replace (head, new_regno))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "\tInsn %d: Renamed %d to %d\n",
		   INSN_UID (insn_info->insn), dest_regno, new_regno);

      return new_regno;
    }

  return -1;
}


/* Return true if REGNO is not safe to rename.  */
static bool
unsafe_rename_p (unsigned regno)
{
  /* Avoid renaming registers used for argument passing and return value.  In
     future we could be a little less conservative and walk through the basic
     blocks to see if there are any call or syscall sites.  */
  if (regno <= R8_REGNUM
      || (regno >= V0_REGNUM && regno < V8_REGNUM))
    return true;

  /* Don't attempt to rename registers that may have specific meanings.  */
  switch (regno)
    {
    case LR_REGNUM:
    case HARD_FRAME_POINTER_REGNUM:
    case FRAME_POINTER_REGNUM:
    case STACK_POINTER_REGNUM:
      return true;
    }

  return false;
}


/* Go through the def/use chains for the register and find the chain for this
   insn to rename.  The function returns the hard register number in case of a
   successful rename and -1 otherwise.  */
static int
rename_dest (tag_insn_info *insn_info, tag_map_t &tag_map)
{
  struct du_chain *chain = NULL;
  du_head_p head = NULL;
  int i;

  unsigned dest_regno = REGNO (insn_info->dest);

  if (unsafe_rename_p (dest_regno))
    return -1;

  /* Search the chain where this instruction is (one of) the root.  */
  rtx_insn *insn = insn_info->insn;
  operand_rr_info *dest_op_info = insn_rr[INSN_UID (insn)].op_info;

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

      for (chain = head->first; chain; chain = chain->next_use)
	/* Found the insn in the chain, so try renaming the register in this
	   chain.  */
	if (chain->insn == insn)
	  return rename_chain (insn_info, tag_map, head);
    }

  return -1;
}


/* Flag to track if the map has changed.  */
static bool map_changed = false;

/* The actual reallocation logic.  For each vector of collisions V, try to
   resolve the collision by attempting to rename the destination register of
   all but one of the loads.  This is a callback that is invoked for each
   name-value pair (T, V) in TAG_MAP.  The function returns true whenever it
   returns unchanged and false otherwise to halt traversal.  */
bool
avoid_collisions_1 (const rtx &t, insn_info_list_t *v, tag_map_t *tag_map)
{
  /* We need at least two loads to cause a tag collision, return unchanged.  */
  if (v->length () < 2)
    return true;

  tag_insn_info *vec_start = v->pop ();
  tag_insn_info *insn_info = vec_start;

  /* Try to rename at least one register to reduce the collision.  If we
     iterate all the way through, we end up dropping one of the loads from the
     list.  This is fine because we want at most one element to ensure that a
     subsequent rename attempt does not end up worsening the collision.  */
  do
    {
      int new_regno;

      if ((new_regno = rename_dest (insn_info, *tag_map)) != -1)
	{
	  rtx new_tag = GEN_INT (TAG_UPDATE_DEST (INTVAL (t), new_regno));

	  tag_map->get_or_insert (new_tag).safe_push (insn_info);
	  df_set_regs_ever_live (new_regno, true);
	  map_changed = true;
	  return false;
	}

      v->safe_insert (0, insn_info);
      insn_info = v->pop ();
    }
  while (insn_info != vec_start);

  if (dump_file)
    fprintf (dump_file, "\t>> Failed to rename destination in insn %d\n\t>>",
	     INSN_UID (insn_info->insn));

  /* Drop the last element and move on to the next tag.  */
  delete insn_info;
  return true;
}


/* For each set of collisions, attempt to rename the registers or insert a move
   to avoid the collision.  We repeatedly traverse through TAG_MAP using
   AVOID_COLLISIONS_1 trying to rename registers to avoid collisions until a
   full traversal results in no change in the map.  */
static void
avoid_collisions (tag_map_t &tag_map)
{
  do
    {
      map_changed = false;
      tag_map.traverse <tag_map_t *, avoid_collisions_1> (&tag_map);
    }
  while (map_changed);
}



/* Find the use def chain in which INSN exists and then see if there is a
   definition inside the loop and outside it.  We use this as a simple
   approximation to determine whether the base register is an IV.  The basic
   idea is to find INSN in the use-def chains for its base register and find
   all definitions that reach it.  Of all these definitions, there should be at
   least one definition that is a simple addition of a constant value, either
   as a binary operation or a pre or post update.

   The function returns true if the base register is estimated to be an IV.  */
static bool
iv_p (rtx_insn *insn, rtx reg, struct loop *loop)
{
  df_ref ause;
  unsigned regno = REGNO (reg);

  /* Ignore loads from the stack.  */
  if (regno == SP_REGNUM)
    return false;

  for (ause = DF_REG_USE_CHAIN (regno); ause; ause = DF_REF_NEXT_REG (ause))
    {
      if (!DF_REF_INSN_INFO (ause)
	  || !NONDEBUG_INSN_P (DF_REF_INSN (ause)))
	continue;

      if (insn != DF_REF_INSN (ause))
	continue;

      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
      df_ref def_rec;

      FOR_EACH_INSN_INFO_DEF (def_rec, insn_info)
	{
	  rtx_insn *insn = DF_REF_INSN (def_rec);
	  basic_block bb = BLOCK_FOR_INSN (insn);

	  if (dominated_by_p (CDI_DOMINATORS, bb, loop->header)
	      && bb->loop_father == loop)
	    {
	      if (recog_memoized (insn) < 0)
		continue;

	      rtx pat = PATTERN (insn);

	      /* Prefetch or clobber; unlikely to be a constant stride.  The
		 falkor software prefetcher tuning is pretty conservative, so
		 its presence indicates that the access pattern is probably
		 strided but most likely with an unknown stride size or a
		 stride size that is quite large.  */
	      if (GET_CODE (pat) != SET)
		continue;

	      rtx x = SET_SRC (pat);
	      if (GET_CODE (x) == ZERO_EXTRACT
		  || GET_CODE (x) == ZERO_EXTEND
		  || GET_CODE (x) == SIGN_EXTEND)
		x = XEXP (x, 0);

	      /* Loading the value from memory; unlikely to be a constant
		 stride.  */
	      if (MEM_P (x))
		continue;

	      /* An increment or decrement by a constant MODE_SIZE amount or
		 the result of a binary expression is likely to be an IV.  */
	      if (GET_CODE (x) == POST_INC
		  || GET_CODE (x) == POST_DEC
		  || GET_CODE (x) == PRE_INC
		  || GET_CODE (x) == PRE_DEC)
		return true;
	      else if (BINARY_P (x)
		       && (CONST_INT_P (XEXP (x, 0))
			   || CONST_INT_P (XEXP (x, 1))))
		{
		  rtx stride = (CONST_INT_P (XEXP (x, 0))
				? XEXP (x, 0) : XEXP (x, 1));

		  /* Don't bother with very long strides because the prefetcher
		     is unable to train on them anyway.  */
		  if (INTVAL (stride) < MAX_PREFETCH_STRIDE)
		    return true;
		}
	    }
	}
      return false;
    }
  return false;
}


/* Return true if SRC is a strided load in the LOOP, false otherwise.
   If it is a strided load, set the BASE and OFFSET.  Also, if this is
   a pre/post increment load, set PRE_POST to true.  */
static bool
valid_src_p (rtx src, rtx_insn *insn, struct loop *loop, bool *pre_post,
	     rtx *base, rtx *offset, bool load_pair)
{
  subrtx_var_iterator::array_type array;
  rtx x = NULL_RTX;

  FOR_EACH_SUBRTX_VAR (iter, array, src, NONCONST)
    if (MEM_P (*iter))
      {
	x = *iter;
	break;
      }

  if (!x)
    return false;

  struct aarch64_address_info addr;
  machine_mode mode = GET_MODE (x);

  if (!aarch64_classify_address (&addr, XEXP (x, 0), mode, true))
    return false;

  if (addr.type != ADDRESS_REG_IMM
      && addr.type != ADDRESS_REG_WB
      && addr.type != ADDRESS_REG_REG
      && addr.type != ADDRESS_REG_UXTW
      && addr.type != ADDRESS_REG_SXTW)
    return false;

  unsigned regno = REGNO (addr.base);
  if (global_regs[regno] || fixed_regs[regno])
    return false;

  if (addr.type == ADDRESS_REG_WB)
    {
      unsigned code = GET_CODE (XEXP (x, 0));

      *pre_post = true;
      *base = addr.base;

      if (code == PRE_MODIFY || code == POST_MODIFY)
	*offset = addr.offset;
      else
	{
	  /*Writeback is only supported for fixed-width modes.  */
	  unsigned int_offset = GET_MODE_SIZE (mode).to_constant ();

	  /* For post-incremented load pairs we would increment the base twice
	     over, so make that adjustment.  */
	  if (load_pair && (code == POST_INC || code == POST_DEC))
	    int_offset *= 2;

	  *offset = GEN_INT (int_offset);
	}
      return true;
    }
  else if (addr.type == ADDRESS_REG_IMM || addr.type == ADDRESS_REG_REG)
    {
      /* Check if the load is strided.  */
      if (!iv_p (insn, addr.base, loop))
	return false;

      *base = addr.base;
      *offset = addr.offset;
      return true;
    }

  return false;
}


/* Return true if INSN is a strided load in LOOP.  If it is a strided load, set
   the DEST, BASE and OFFSET.  Also, if this is a pre/post increment load, set
   PRE_POST to true.

   The routine does checks on the destination of the insn and depends on
   STRIDED_LOAD_P to check the source and fill in the BASE and OFFSET.  */
static bool
get_load_info (rtx_insn *insn, struct loop *loop, rtx *dest, rtx *base,
	       rtx *offset, bool *pre_post, bool *ldp)
{
  if (!INSN_P (insn) || recog_memoized (insn) < 0)
    return false;

  rtx pat = PATTERN (insn);
  unsigned code = GET_CODE (pat);
  bool load_pair = (code == PARALLEL);

  /* For a load pair we need only the first base and destination
     registers.  We however need to ensure that our pre/post increment
     offset is doubled; we do that in STRIDED_LOAD_P.  */
  if (load_pair)
    {
      pat = XVECEXP (pat, 0, 0);
      code = GET_CODE (pat);
    }

  if (code != SET)
    return false;

  rtx dest_rtx = SET_DEST (pat);

  if (!REG_P (dest_rtx))
    return false;

  unsigned regno = REGNO (dest_rtx);
  machine_mode mode = GET_MODE (dest_rtx);
  machine_mode inner_mode = GET_MODE_INNER (mode);

  /* Falkor does not support SVE vectors.  */
  if (!GET_MODE_SIZE (mode).is_constant ())
    return false;

  /* Ignore vector struct or lane loads.  */
  if (GET_MODE_SIZE (mode).to_constant ()
      != GET_MODE_SIZE (inner_mode).to_constant ())
    return false;

  /* The largest width we want to bother with is a load of a pair of
     quad-words.  */
  if ((GET_MODE_SIZE (mode).to_constant () << load_pair)
      > GET_MODE_SIZE (OImode))
    return false;

  /* Ignore loads into the stack pointer because it is unlikely to be a
     stream.  */
  if (regno == SP_REGNUM)
    return false;

  if (valid_src_p (SET_SRC (pat), insn, loop, pre_post, base, offset,
		   load_pair))
    {
      *dest = dest_rtx;
      *ldp = load_pair;

      return true;
    }

  return false;
}


/* Return whether INSN and CAND are in the same def/use chain.  */
static bool
in_same_chain (rtx_insn *insn, rtx_insn *cand, unsigned regno)
{
  struct du_chain *chain = NULL;
  du_head_p head = NULL;
  int i;

  /* Search the chain where this instruction is (one of) the root.  */
  operand_rr_info *op_info = insn_rr[INSN_UID (insn)].op_info;

  for (i = 0; i < op_info->n_chains; i++)
    {
      /* The register tracked by this chain does not match the
	 dest register of insn.  */
      if (op_info->heads[i]->regno != regno)
	continue;

      head = op_info->heads[i];
      /* The chain was merged in another, find the new head.  */
      if (!head->first)
	head = regrename_chain_from_id (head->id);

      bool found_insn = false, found_cand = false;

      for (chain = head->first; chain; chain = chain->next_use)
	{
	  rtx *loc = &SET_DEST (PATTERN (chain->insn));

	  if (chain->loc != loc)
	    continue;

	  if (chain->insn == insn)
	    found_insn = true;

	  if (chain->insn == cand)
	    found_cand = true;

	  if (found_insn && found_cand)
	    return true;
	}
    }

  return false;
}


/* Callback function to traverse the tag map and drop loads that have the same
   destination and are in the same chain of occurrence.  Routine always returns
   true to allow traversal through all of TAG_MAP.  */
bool
single_dest_per_chain (const rtx &t ATTRIBUTE_UNUSED, insn_info_list_t *v,
		       void *arg ATTRIBUTE_UNUSED)
{
  for (int i = v->length () - 1; i>= 1; i--)
    {
      tag_insn_info *insn_info = (*v)[i];

      for (int j = v->length () - 2; j >= 0; j--)
	{
	  /* Filter out destinations in the same chain.  */
	  if (in_same_chain (insn_info->insn, (*v)[j]->insn,
			     REGNO (insn_info->dest)))
	    {
	      v->ordered_remove (j);
	      i = v->length ();
	      break;
	    }
	}
    }

  return true;
}


/* Callback invoked for each name-value pair (T, INSN_INFO) to dump the insn
   list INSN_INFO for tag T.  */
bool
dump_insn_list (const rtx &t, const insn_info_list_t &insn_info,
		void *unused ATTRIBUTE_UNUSED)
{
  gcc_assert (dump_file);
  fprintf (dump_file, "Tag 0x%lx ::\n", INTVAL (t));

  for (unsigned i = 0; i < insn_info.length (); i++)
    dump_insn_slim (dump_file, insn_info[i]->insn);

  fprintf (dump_file, "\n");

  return true;
}


/* Record all loads in LOOP into TAG_MAP indexed by the falkor hardware
   prefetcher memory tags.  */
static void
record_loads (tag_map_t &tag_map, struct loop *loop)
{
  rtx_insn *insn;
  basic_block *body, bb;

  body = get_loop_body (loop);

  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      bb = body[i];
      FOR_BB_INSNS (bb, insn)
	{
	  rtx base = NULL_RTX;
	  rtx dest = NULL_RTX;
	  rtx offset = NULL_RTX;
	  bool writeback = false;
	  bool ldp = false;

	  if (!INSN_P (insn) || DEBUG_INSN_P (insn))
	    continue;

	  if (get_load_info (insn, loop, &dest, &base, &offset, &writeback,
			     &ldp))
	    {
	      tag_insn_info *i = new tag_insn_info (insn, dest, base, offset,
						    writeback, ldp);
	      rtx tag = GEN_INT (i->tag ());
	      tag_map.get_or_insert (tag).safe_push (i);
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "Loop %d: Tag map generated.\n", loop->num);
      tag_map.traverse <void *, dump_insn_list> (NULL);
    }

  /* Try to reduce the dataset before launching into the rename attempt.  Drop
     destinations in the same collision chain that appear in the same def/use
     chain, all as defs.  These chains will move together in a rename so
     there's no point in keeping both in there.  */
  tag_map.traverse <void *, single_dest_per_chain> (NULL);
}


/* Tag collision avoidance pass for Falkor.  The pass runs in two phases for
   each loop; the first phase collects all loads that we consider as
   interesting for renaming into a tag-indexed map of lists.  The second phase
   renames the destination register of the loads in an attempt to spread out
   the loads into different tags.  */
void
execute_tag_collision_avoidance ()
{
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_compute_regs_ever_live (true);
  df_note_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  regrename_init (true);
  regrename_analyze (NULL);

  compute_bb_for_insn ();
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      tag_map_t tag_map (512);

      record_loads (tag_map, loop);
      avoid_collisions (tag_map);
      if (dump_file)
	{
	  fprintf (dump_file, "Loop %d: Completed rename.\n", loop->num);
	  tag_map.traverse <void *, dump_insn_list> (NULL);
	}
      tag_map.traverse <void *, free_insn_info> (NULL);
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  regrename_finish ();
}


const pass_data pass_data_tag_collision_avoidance =
{
  RTL_PASS, /* type */
  "tag_collision_avoidance", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};


class pass_tag_collision_avoidance : public rtl_opt_pass
{
public:
  pass_tag_collision_avoidance (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_tag_collision_avoidance, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return ((aarch64_tune_params.extra_tuning_flags
	       & AARCH64_EXTRA_TUNE_RENAME_LOAD_REGS)
	      && optimize >= 2);
    }

  virtual unsigned int execute (function *)
    {
      execute_tag_collision_avoidance ();
      return 0;
    }

}; // class pass_tag_collision_avoidance


/* Create a new pass instance.  */
rtl_opt_pass *
make_pass_tag_collision_avoidance (gcc::context *ctxt)
{
  return new pass_tag_collision_avoidance (ctxt);
}
