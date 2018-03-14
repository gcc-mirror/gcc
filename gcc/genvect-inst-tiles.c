/* Loop Vectorization using unified representation for permute instructions.
   Copyright (C) 2003-2015 Free Software Foundation, Inc.
   Contributed by Sameera Deshpande <sameera.deshpande@imgtec.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define GENERATOR_FILE 1
#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#ifdef GENERATOR_FILE
#include "machmode.h"
#include "signop.h"
#include "wide-int.h"
#include "double-int.h"
#include "real.h"
#include "fixed-value.h"
#include "statistics.h"
#include "vec.h"
#include "hash-table.h"
#include "hash-set.h"
#include "input.h"
#include "is-a.h"
#include "target.h"
#endif

#include "tree-core.h"
#include "tree-vect-unified.h"
//#include "tree-vect-unified-common.c"
//#include "tree-vect-unified-opts.c"

#define DEBUG 0
int target_flags;

enum rule_type {NT2T, NT2NT, NT2OP};

/* Normalized context free grammar of the form
   NT --> T
   NT--> NT
   NT --> OP (<list of NTs>)  */
struct grammar_rule
{
  /* Pointer to vec_perm_order_spec corresponding to grammar rule.  For default
     rules, this value is NULL.  */
  struct vec_perm_order_spec *porder;

  /* Non-terminal on LHS.  */
  int lhs_nt;

  enum rule_type type;

  int spec_idx;

  int cost;

  union {
    /* Terminal on RHS.  */
    int terminal;

    /* Non-terminal on RHS.  */
    int non_terminal;

    /* RHS of the form OP_div,sel (NT1, NT2...NTk) for k_arity operation op.  */
    struct rhs_expression {
      struct operation {
	enum primop_code op;
	int opd_selector;
	int division;
	int out_type;
	tree *var_stride;
      } primop;

      vec<int> rhs_nt;
    }  rhs_exp;
  } u;
};

struct vec_perm_order_spec target_spec[] = TARGET_VEC_PERM_CONST_ORDER;
vec <struct grammar_rule *> rules;

int default_extr_2_0, default_extr_2_1, default_ilv_2;

int name_idx = 0;

struct non_terminal
{
  char *str;
  vec <int> nt_on_lhs_rules;
  vec <int> nt_on_rhs_rules;
  int state;
  int type;
};

struct terminal
{
  char *str;
  int state;
  int type;
};

/* List of non-terminals used in grammar.  The index is used in the grammar rule
   to point to appropriate non-terminal in the list.  For now, the non-terminal
   is just list of strings with NT names.  However if needed, it can be updated
   to hold additional information in the structure.  */
vec<struct non_terminal *> non_terminals;

/* List of terminals in Grammar.  Currently, we support only 3 categories in
   terminals -
   MEM, REG and CONST.  */
vec<struct terminal *>terminals;

struct transition_state
{
  int id;
  vec<int> nt;
  vec<int> rule;
  vec<int> cost;
};

vec<struct transition_state *> states;
vec<int> arity_list;

struct operator_info
{
  /* Permute operation.  */
  enum primop_code pcode;

  /* Arity of operator.  */
  int arity;

  /* Actual arity of operator.  */
  int act_arity;

  /* Selector of the operator.  */
  int sel;

  /* Type of operator.  */
  int type;

  /* Projection Map for ith operand of operator w.r.t. state.  */
  vec<struct transition_state *> map[10]; 
  vec<long long> index_map;
  vec<int>trans_map;
  vec<int> state_map;
  vec<struct transition_state *> rep_states[10];
};

/* Function create_placeholder.

*/

struct primop_tree *
create_placeholder (int idx, char ch, struct primop_tree *parent)
{
  struct primop_tree *ptree;

  ptree = populate_prim_node (POP_PH, NULL,
			      parent, NULL, NULL);
  PT_PH_IDX (ptree) = idx;
  PT_PH_TYPE (ptree) = ch;
  return ptree;
}

/* Function create_perm_order_tree.

   For each element in TARGET_VEC_PERM_CONST_ORDER
   Do
     1. Create ILV node with arity out_vec_size.
     2. For ith element in perm_order
	Do
	  1. Create EXTR node with parts = in_vec_size and selector = i % parts
	  2. Create child of EXTR as PLACEHOLDER_<M|C|R>_<i / parts>, i / parts
   	     should not exceed num_opd.  For k_arity_promotion_reduction and
   	     unity_redundancy_elimination, PLACEHOLDER_<M|C|R>_<num> is used for
   	     matching.  Whereas for grammar definition, only PLACEHOLDER_<M|C|R>
   	     is used for generating rules.
	   Done
   Done
*/

struct primop_tree *
create_perm_order_tree (struct vec_perm_order_spec spec)
{
  int i, num;
  struct primop_tree *ilv_node, *expr_node, *placeholder;

  ilv_node = create_primTree_combine (POP_ILV, NULL,
		spec.out_vec_size, NULL, NULL, NULL);

  for (i = 0; i < spec.out_vec_size; i++)
    {
      expr_node = create_primTree_partition (POP_EXTR, NULL,
			spec.in_vec_size, spec.perm_order[i] % spec.in_vec_size,
			NULL, ilv_node, NULL);
      num = spec.perm_order[i] / spec.in_vec_size;
      placeholder = create_placeholder (num,
			 spec.opd_constraint[num], expr_node);
      add_child_at_index (expr_node, placeholder, 0);
      add_child_at_index (ilv_node, expr_node, i);
    }

  return ilv_node;
}

/* Function print_perm_order.

*/

void print_perm_order (int *perm_order, int num)
{
  int i;

  for (i = 0; i < num; i++)
    printf (" %d ", perm_order[i]);
}

/* Function print_instruction_tile.

*/

void
print_instruction_tile (struct primop_tree *ptree, int tab = 0)
{
  int i;

  if (PT_NODE_OP (ptree) != POP_PH)
    {
      printf ("\n");
      for (i = 0; i < tab; i++)
	printf (" ");
    }
  switch (PT_NODE_OP (ptree))
    {
      case POP_EXTR:
	printf ("EXTR_%d,%d (", PT_DIVISION (ptree),
		 PT_OPERAND_SELECTOR (ptree));
	print_instruction_tile (PT_CHILD (ptree, 0), tab + 2);
	printf (")");
	break;
      case POP_ILV:
	printf ("ILV_%d (", PT_DIVISION (ptree));
	for (i = 0; i < PT_DIVISION (ptree) - 1; i++)
	  {
	    print_instruction_tile (PT_CHILD (ptree, i), tab + 2);
	    printf (" , ");
	  }
	print_instruction_tile (PT_CHILD (ptree, i), tab + 2);
	printf (")");
	break;
      case POP_PH:
	printf ("PH%c:%d", PT_PH_TYPE (ptree), PT_PH_IDX (ptree));
	break;
      default:
	gcc_assert (!"\nUndesired case in printing tree.\n");
	return;
    }
}

/* Function print_instruction_tiles.

*/

void
print_instruction_tiles ()
{
  int i;
  printf ("/*");
  for (i = 0; i < sizeof (target_spec)/sizeof (struct vec_perm_order_spec); i++)
    {
      printf ("\n\npermute order - ");
      print_perm_order (target_spec[i].perm_order, target_spec[i].out_vec_size);
      print_instruction_tile (target_spec[i].ptree);
    }
  printf ("*/\n\n");
}

/* Function create_instruction_tiles.

   For each permute_order in TARGET_VEC_PERM_CONST_ORDER
   Do
     1. Create permute order tree from permute order - the permute order tree
	so created is of arity out_vec_size.
     2. Perform k_arity_promotion_reduction on permute order tree to reduce the
	arity to 2.  As out_vec_size is power of 2, the promotion/reduction is
	never going to fail.
     3. Perform unity_redundancy_elimination of kind
	ILV_m (EXTR_0(S), EXTR_1(S),...EXTR_m-1(S)) => S
	EXTR_m,x (ILV_M(S1, S2, ... Sm)) => Sx
	to get optimal permute order tree.
   Done
*/

void
create_instruction_tiles ()
{
  int i;
  struct primop_tree *ptree;
  arity_list = vNULL;
  arity_list.safe_insert (0, 2);

  for (i = 0; i < sizeof (target_spec)/sizeof (struct vec_perm_order_spec); i++)
    {
      ptree = create_perm_order_tree (target_spec[i]);
      ptree = k_arity_promotion_reduction (ptree, 2);
      ptree = unity_redundancy_elimination (ptree);
      target_spec[i].ptree = ptree;
    }

}

/* Function get_term_index.

   Return index of terminal.
*/

int
get_term_index (vec<struct terminal *> *worklist, char *str, int type)
{
  int i;
  for (i = 0; i < worklist->length (); i++)
    {
      if (type == (*worklist)[i]->type && !strcmp ((*worklist)[i]->str, str))
	return i;
    }
  return -1;
}


/* Function get_index.

   Return index of non-terminal.
*/

int
get_index (vec<struct non_terminal *> *worklist, char *str, int type)
{
  int i;
  for (i = 0; i < worklist->length (); i++)
    {
      if (type == (*worklist)[i]->type && !strcmp ((*worklist)[i]->str, str))
	return i;
    }
  return -1;
}

/* Function create_non_terminal.

*/

int
create_non_terminal (char *str, int type)
{
  int idx;
  struct non_terminal *buf;

  idx = get_index (&non_terminals, str, type);
  if (idx != -1)
    return idx;

  idx = non_terminals.length ();
  buf = (struct non_terminal *) xcalloc (1, sizeof (struct non_terminal));
  buf->str = (char *) xcalloc (strlen (str), sizeof (char));
  strcpy (buf->str, str);
  buf->nt_on_lhs_rules = vNULL;
  buf->nt_on_rhs_rules = vNULL;
  buf->state = -1;
  non_terminals.safe_insert (idx, buf);
  non_terminals[idx]->type = type;
  return idx;
}

/* Function create_rule_NT_to_NT.

   Creates grammar rule of kind NT --> NT for normalized grammar.
*/

int
create_rule_NT_to_NT (int lhs_nt, int rhs_nt, int off, int cost)
{
  struct grammar_rule * rule;
  int ruleno;

  gcc_assert (non_terminals[lhs_nt]->type == non_terminals[rhs_nt]->type);
  rule = (struct grammar_rule *) xcalloc (1, sizeof (struct grammar_rule));
  rule->lhs_nt = lhs_nt;
  rule->type = NT2NT;
  rule->u.non_terminal = rhs_nt;
  rule->spec_idx = off;
  if (off != -1)
    rule->cost = target_spec[off].cost;
  else
    rule->cost = cost;
  ruleno = rules.length ();
  rules.safe_insert (ruleno, rule);
  non_terminals[lhs_nt]->nt_on_lhs_rules.safe_insert (
	non_terminals[lhs_nt]->nt_on_lhs_rules.length (), ruleno);
  
  non_terminals[rhs_nt]->nt_on_rhs_rules.safe_insert (
	non_terminals[rhs_nt]->nt_on_rhs_rules.length (), ruleno);

  return ruleno;
}

int lookup_NT2T_in_grammar (vec <struct grammar_rule *> *, char *, int);
/* Function create_rule_NT_to_T.

   Creates grammar rule of kind NT --> T for normalized grammar.
*/

int
create_rule_NT_to_T (int lhs_nt, int rhs_t, int off, int cost = 8)
{
  struct grammar_rule * rule;
  int ruleno;

  ruleno = lookup_NT2T_in_grammar (&rules, non_terminals[lhs_nt]->str, rhs_t);
  if (ruleno != -1)
    return ruleno;

  gcc_assert (non_terminals[lhs_nt]->type == terminals[rhs_t]->type);
  rule = (struct grammar_rule *) xcalloc (1, sizeof (struct grammar_rule));
  rule->lhs_nt = lhs_nt;
  rule->type = NT2T;
  rule->u.terminal = rhs_t;
  rule->spec_idx = off;
  if (off != -1)
    rule->cost = target_spec[off].cost;
  else
    rule->cost = cost;

  ruleno = rules.length ();
  rules.safe_insert (ruleno, rule);

  non_terminals[lhs_nt]->nt_on_lhs_rules.safe_insert (
	non_terminals[lhs_nt]->nt_on_lhs_rules.length (), ruleno);
  return ruleno;
}

/* Function create_rule_NT_to_op_tree.

   Creates grammar rule of kind NT --> OP (NT1, NT2 ...) for normalized grammar.
*/

int
create_rule_NT_to_op_tree (int lhs_nt, enum primop_code op, int selector,
 			   int division, int *rhs_opd, int length, int off,
			   int otype, int cost=-1)
{
  struct grammar_rule * rule;
  int ruleno;
  int i;

  rule = (struct grammar_rule *) xcalloc (1, sizeof (struct grammar_rule));
  rule->lhs_nt = lhs_nt;
  rule->type = NT2OP;
  rule->u.rhs_exp.primop.op = op;
  rule->u.rhs_exp.primop.opd_selector = selector;
  rule->u.rhs_exp.primop.division = division;
  rule->u.rhs_exp.primop.out_type = otype;
  rule->u.rhs_exp.primop.var_stride = NULL;
  rule->spec_idx = off;
  if (off != -1)
    rule->cost = target_spec[off].cost;
  else if (cost != -1)
    rule->cost = cost;
  else
    rule->cost = (op == POP_EXTR ? 8 : 16);//rules[default_ilv_2]->cost);

  for (i = 0; i < length; i++)
    {
      rule->u.rhs_exp.rhs_nt.safe_insert (i, rhs_opd[i]);
    }

  ruleno = rules.length ();
  rules.safe_insert (ruleno, rule);

  non_terminals[lhs_nt]->nt_on_lhs_rules.safe_insert (
	non_terminals[lhs_nt]->nt_on_lhs_rules.length (), ruleno);

  return ruleno;
}

int
create_terminal (char *str, int type)
{
  struct terminal *buf;
  int idx;

  idx = get_term_index (&terminals, str, type);
  if (idx != -1)
    return idx;
  buf = (struct terminal *) xcalloc (1, sizeof (struct terminal));
  buf->str = (char *) xcalloc (strlen(str)+1, sizeof (char));
  strcpy (buf->str, str);
  buf->state = -1;
  idx = terminals.length ();
  terminals.safe_insert (idx, buf);
  terminals[idx]->type = type;

  return idx;
}
vec<char *> type_name = vNULL;

int
lookup_type (vec<char *> *type_name, char *type)
{
  int i;
  for (i = 0; i < type_name->length (); i++)
    {
      if (!strcmp (type, (*type_name)[i]))
	return i;
    }
  char *str = (char *) xcalloc (1, strlen (type));
  strcpy (str, type);
  type_name->safe_insert (i, str);
  return i;
}
/* Function create_terminals.

*/

void
create_terminals ()
{
  struct terminal *buf;
  int idx, mem, consti;
  int vec_size, vector_sizes, max_vec_size, i;
  int type;

  vector_sizes = MAX_VECTOR_SIZE;
  max_vec_size = 1 << floor_log2 (vector_sizes);
  vec_size = max_vec_size;

//  for (i = 0; i < floor_log2 (max_vec_size); i++)
  for (i = 0; i < sizeof (target_spec)/sizeof (struct vec_perm_order_spec); i++)
    {
      char buf[20];
      int idx;

      type = lookup_type (&type_name, target_spec[i].type);
      sprintf (buf, "MEM_%s", target_spec[i].type);
      mem = create_terminal (buf, type);
      sprintf (buf, "mem_%s", target_spec[i].type);
      create_rule_NT_to_T (create_non_terminal (buf, type), mem, -1);

      sprintf (buf, "CONST_%s", target_spec[i].type);
      consti = create_terminal (buf, type);
      sprintf (buf, "const_%s", target_spec[i].type);
      create_rule_NT_to_T (create_non_terminal (buf, type), consti, -1);


      sprintf (buf, "REG_%s", target_spec[i].type);
      idx = create_terminal (buf, type);
      sprintf (buf, "reg_%s", target_spec[i].type);
      create_rule_NT_to_T (create_non_terminal (buf, type), idx, -1);
      create_rule_NT_to_T (create_non_terminal (buf, type), mem, -1);
      create_rule_NT_to_T (create_non_terminal (buf, type), consti, -1);
    }

  return;
}

/* Function create_default_rules.

     Default rules		Costs
     ================================
     goal --> reg     		(0)
     goal --> mem     		(0)
     reg --> mem  		(10)
     reg --> const 		(5)
     mem --> reg		(10)
     mem --> const		(8)
       :
       :
     reg_32 --> REG		(0)
     reg_16 --> REG		(0)
     reg_8 --> REG		(0)
     reg_4 --> REG		(0)
     reg_2 --> REG		(0)
     reg -> reg_2		(0)
     reg -> reg_4		(0)
     reg -> reg_8		(0)
     reg -> reg_16		(0)
     reg -> reg_32		(0)
       :
       :
     mem --> MEM		(0)
     const --> CONST		(0)
     reg --> EXTR_2,0 (reg)	(1)
     reg --> EXTR_2,1 (reg)	(1)
     reg --> ILV (reg, reg)	(2)
*/

void
create_default_rules ()
{
  int goal, reg, mem, consti;
  int i, vec_size, max_vec_size, vector_sizes;
  int v[2];

  create_terminals ();
  return;
}

/* Function lookup_NT2T_in_grammar.

   Look-up similar rule in rule-list.
*/

int
lookup_NT2T_in_grammar (vec <struct grammar_rule *> *rule,
			char *nt_substr, int t_idx)
{
  int i;

  if (*rule == vNULL)
    return -1;

  for (i = 0; i < rule->length (); i++)
    {
      struct grammar_rule *r;
      rule->iterate (i, &r);

      if (r->u.terminal == t_idx
	  && (strstr (non_terminals[r->lhs_nt]->str, nt_substr)))
	return i;
    }
  return -1;
}

/* Function lookup_NT2OP_in_grammar.

   Look-up rule matching operation OP and children in vector nt_list.
*/

int
lookup_NT2OP_in_grammar (vec <struct grammar_rule *> *rule,
			 enum primop_code code, int sel, int div, int otype,
			 int *nt_list, int length)
{
  int i, j;

  if (*rule == vNULL)
    return -1;

  for (i = 0; i < rule->length (); i++)
    {
      struct grammar_rule *r;
      rule->iterate (i, &r);

      if (r->u.rhs_exp.primop.op == code
	  && sel == r->u.rhs_exp.primop.opd_selector
	  && div == r->u.rhs_exp.primop.division
	  && otype == r->u.rhs_exp.primop.out_type)
	{
	  for (j = 0; j < length; j++)
	    {
	      if (nt_list[j] != r->u.rhs_exp.rhs_nt[j])
		break;
	    }
	  if (j == length)
	    return i;
	}
    }

  return -1;
}

/* Function create_rule_for_ptree.

   Recursive function to create grammar rules in normal form.

   If the leaf node with placeholder :
     - check if the rule is already present.
       If yes, return the previously created non-terminal
       Otherwise, create new non-terminal for the place-holder, with appropriate
       vector size, and create rules of the form
	 <new_nt> --> <REG|MEM|CONST>
	 <reg_<vecsize>|mem|const> --> <new_nt>

   If non-leaf node,
     - For each child of the node invoke the function recursively.
     - Once all children are processed, check if the rule with current primop
       and children is already present.
       If yes, return previously created non-terminal for corresponding rule.
       Otherwise, create non-terminal for out_vecsize and create rules of the
       form
	 <new_nt> --> PRIMOP (<list of NTs corresponding to respective child>)
	 <reg_<vecsize>|mem|const> --> <new_nt>
*/

int
create_rule_for_ptree (struct primop_tree *ptree, int spec_idx, int out_vecsize,
		       int in_vecsize, int target_type, int type)
{
  int chld_nt[30];
  char buf[20], name[20], buf1[20], buf2[20];
  int found, nt, new_in_vec_size, i, ruleno;

  if (PT_NODE_OP (ptree) == POP_PH)
    {
	  sprintf (buf, "reg_%s", type_name[type]);
	  sprintf (buf1, "mem_%s", type_name[type]);
	  sprintf (buf2, "const_%s", type_name[type]);
	  return create_non_terminal (PT_PH_TYPE (ptree) == 'R' ? (char *) buf
			: PT_PH_TYPE (ptree) == 'M' ? (char *) buf1
			: (char *)buf2, type);
    }

  if (PT_NODE_OP (ptree) == POP_ILV)
    new_in_vec_size = in_vecsize / PT_DIVISION (ptree);
  if (PT_NODE_OP (ptree) == POP_EXTR)
    new_in_vec_size = in_vecsize * PT_DIVISION (ptree);

  for (i = 0; i < ptree->children.length (); i++)
    {
      chld_nt[i] = create_rule_for_ptree (PT_CHILD (ptree, i),
			-1, in_vecsize, new_in_vec_size, target_type, type);
    }


  found = lookup_NT2OP_in_grammar (&rules,
				   (enum primop_code) PT_NODE_OP (ptree),
				   PT_OPERAND_SELECTOR (ptree),
				   PT_DIVISION (ptree), type, chld_nt, i);

  if (found != -1)
    {
      return (rules)[found]->lhs_nt;
    }
  else
    {
      /* Create new NT, and create rule NT_to_OP.  */
      sprintf (name, "inter%d", name_idx++);
      nt = create_non_terminal (name, type);
      ruleno = create_rule_NT_to_op_tree (nt,
		 (enum primop_code) PT_NODE_OP (ptree),
    		 PT_OPERAND_SELECTOR (ptree), PT_DIVISION (ptree),
    		 chld_nt, i, spec_idx, type);

      if (spec_idx == -1) 
	{
          //ruleno = create_rule_NT_to_NT (create_non_terminal ("reg"), nt, -1, 0);
        }

     if (spec_idx != -1)
	{
	  sprintf (buf, "reg_%s", type_name[type]);

	  ruleno = create_rule_NT_to_NT (
    				create_non_terminal (buf, type),
			    	nt, -1, 0);
	}
      return nt;
    }
}

void
create_default_op_rules ()
{
  int goal, reg, mem, consti;
  int i, j, vec_size, max_vec_size, vector_sizes;
  int v[2];
  int chld_nt[4];
  char name[20];
  int found, nt, ruleno;


  /* For each vector type supported, add NT2T rule for
     reg.  */
  vector_sizes = MAX_VECTOR_SIZE;
  max_vec_size = 1 << floor_log2 (vector_sizes);
  vec_size = max_vec_size >> 1;

  for (i = 0; i < type_name.length (); i++)
  {
    char buf[20];
    int idx;

    sprintf (buf, "reg_%s", type_name[i]);
    idx = create_non_terminal (buf, i);

   for (j = 0; j < 2; j++)
    {
      chld_nt[j] = idx;
    }


    found = lookup_NT2OP_in_grammar (&rules, POP_ILV, -1,
				   2, i, chld_nt, 2);

    if (found == -1)
      {
        /* Create new NT, and create rule NT_to_OP.  */
        sprintf (name, "inter%d", name_idx++);
        nt = create_non_terminal (name, i);
        ruleno = create_rule_NT_to_op_tree (nt, POP_ILV, -1, 2,
	  	      chld_nt, 2, -1, i);

        ruleno = create_rule_NT_to_NT (idx, nt, -1, 0);
      }
    else
      {
        ruleno = create_rule_NT_to_NT (idx, rules[found]->lhs_nt, -1, 0);
      }

    found = lookup_NT2OP_in_grammar (&rules, POP_EXTR, 0,
				   2, i, chld_nt, 1);

    if (found == -1)
      {
        /* Create new NT, and create rule NT_to_OP.  */
        sprintf (name, "inter%d", name_idx++);
        nt = create_non_terminal (name, i);
        ruleno = create_rule_NT_to_op_tree (nt, POP_EXTR, 0, 2,
	 	      chld_nt, 1, -1, i);

        ruleno = create_rule_NT_to_NT (idx, nt, -1, 0);
      }
    else
      {
        ruleno = create_rule_NT_to_NT (idx, rules[found]->lhs_nt, -1, 0);
      }

    found = lookup_NT2OP_in_grammar (&rules, POP_EXTR, 1,
				   2, i, chld_nt, 1);

    if (found == -1)
      {
        /* Create new NT, and create rule NT_to_OP.  */
        sprintf (name, "inter%d", name_idx++);
        nt = create_non_terminal (name, i);
        ruleno = create_rule_NT_to_op_tree (nt, POP_EXTR, 1, 2,
		      chld_nt, 1, -1, i);

        ruleno = create_rule_NT_to_NT (idx, nt, -1, 0);
      }
    else
      {
        ruleno = create_rule_NT_to_NT (idx, rules[found]->lhs_nt, -1, 0);
      }

  }
  return;

}

/* Function create_grammar_rules.

   Creates grammar rules for each primop_tree.
*/

void
create_grammar_rules ()
{
  int i;

  rules = vNULL;
  non_terminals = vNULL;
  terminals = vNULL;
  create_default_rules ();
  for (i = 0;
       i < sizeof (target_spec)/sizeof (struct vec_perm_order_spec);
       i++)
    {
      int idx = 0;

      create_rule_for_ptree (target_spec[i].ptree, i,
		 target_spec[i].out_vec_size,
		 target_spec[i].in_vec_size,
		 target_spec[i].opd_constraint[0],
		 lookup_type(&type_name, (target_spec[i].type)));
    }
  create_default_op_rules ();
}

/* Function print_rule_operands.

*/

void
print_rule_operands (vec<int> *arr)
{
  int i;
  printf ("%s", non_terminals[(*arr)[0]]->str);
  for (i = 1; i < arr->length (); i++)
    {
      printf (", %s", non_terminals[(*arr)[i]]->str);
    }
}

/* Function print_grammar_rule.

*/

void
print_grammar_rule (struct grammar_rule *rule)
{
  switch (rule->type)
    {
      case NT2T:
	printf ("%s --> %s", non_terminals[rule->lhs_nt]->str,
		 terminals[rule->u.terminal]->str);
	break;

      case NT2NT:
	printf ("%s --> %s", non_terminals[rule->lhs_nt]->str,
		 non_terminals[rule->u.non_terminal]->str);

	break;

      case NT2OP:
	printf ("%s --> ", non_terminals[rule->lhs_nt]->str);
	switch (rule->u.rhs_exp.primop.op)
	  {
	    case POP_ILV:
	      printf ("ILV_%d_%s (", rule->u.rhs_exp.primop.division,
			      type_name[rule->u.rhs_exp.primop.out_type]);
	      break;
	    case POP_EXTR:
	      printf ("EXTR_%d,%d_%s (", rule->u.rhs_exp.primop.division,
		      rule->u.rhs_exp.primop.opd_selector,
		      type_name[rule->u.rhs_exp.primop.out_type]);
	      break;
	    default:
	      gcc_assert (0);
	  }
	print_rule_operands (&rule->u.rhs_exp.rhs_nt);
	printf (")");

	break;

      default:
	gcc_assert (0);
    }

  if (rule->spec_idx != -1)
    printf (" : [%d::%d]", rule->spec_idx, rule->cost);
}

void
print_grammar_rules_in_comment ()
{
  int i,j;

printf ("/*\n");
  for (i = 0; i < rules.length (); i++)
    {
      printf ("\n%d:\t", i);
      print_grammar_rule (rules[i]);
    }
  printf ("*/\n\n");
}


void
print_grammar_rules ()
{
  int i,j;

  printf ("enum rule_type {NT2T, NT2NT, NT2OP};\n\n");
  printf ("struct grammar_rule\n{\n");
  printf ("  int lhs;\n  enum rule_type type;\n  int spec_idx;\n");
  printf ("  union\n {\n");
  printf ("    int terminal;\n    int non_terminal;\n");
  printf ("    struct\n    {\n");
  printf ("      enum primop_code op;\n      int selector;\n");
  printf ("      int division;\n      int out_type;\n");
  printf ("      vec<int> opd;\n");
  printf ("    } rhs_exp;\n");
  printf ("  } u;\n");
  printf ("};\n\n");
  printf ("vec<struct grammar_rule *> rules = vNULL;\n\n");

  printf ("void\ninit_grammar_rules ()\n{\n");
  printf ("  struct grammar_rule *rule;\n\n");

  for (i = 0; i < rules.length (); i++)
    {
      printf ("  rule = (struct grammar_rule *)");
      printf (" xcalloc (1, sizeof (struct grammar_rule));\n");
       printf ("  /* %d: ", i);
      print_grammar_rule (rules[i]);
      printf ("  */\n");
     switch (rules[i]->type)
	{
	case NT2T:
	  printf ("  rule->type = NT2T;\n");
	  printf ("  rule->lhs = UNIF_VECT_NT_%s;\n",
		non_terminals[rules[i]->lhs_nt]->str);
	  printf ("  rule->u.terminal = UNIF_VECT_T_%s;\n",
		terminals[rules[i]->u.terminal]->str);
	  break;
	case NT2NT:
	  printf ("  rule->type = NT2NT;\n");
	  printf ("  rule->lhs = UNIF_VECT_NT_%s;\n",
		non_terminals[rules[i]->lhs_nt]->str);
	  printf ("  rule->u.non_terminal = UNIF_VECT_NT_%s;\n",
		non_terminals[rules[i]->u.non_terminal]->str);
	  break;
	case NT2OP:
	  printf ("  rule->type = NT2OP;\n");
	  printf ("  rule->lhs = UNIF_VECT_NT_%s;\n",
	        non_terminals[rules[i]->lhs_nt]->str);
	  printf ("  rule->u.rhs_exp.op = POP_%s;\n",
		tree_code_name[rules[i]->u.rhs_exp.primop.op]);
	  printf ("  rule->u.rhs_exp.selector = %d;\n",
		rules[i]->u.rhs_exp.primop.opd_selector);
	  printf ("  rule->u.rhs_exp.division = %d;\n",
		rules[i]->u.rhs_exp.primop.division);
	  printf ("  rule->u.rhs_exp.out_type = %d;\n",
		rules[i]->u.rhs_exp.primop.out_type);
	  printf ("  rule->u.rhs_exp.opd = vNULL;\n");
	  for (j = 0; j < rules[i]->u.rhs_exp.rhs_nt.length (); j++)
	    {
	      printf ("  rule->u.rhs_exp.opd.safe_insert (");
	      printf ("  rule->u.rhs_exp.opd.length (), UNIF_VECT_NT_%s);\n",
		non_terminals[rules[i]->u.rhs_exp.rhs_nt[j]]->str);
	    }
	  break;
	default:
	  gcc_assert (!"Unknown rule.");
	}
	  printf ("  rule->spec_idx = %d;\n", rules[i]->spec_idx);
	  printf ("  rules.safe_insert (rules.length (), rule);\n\n");
    }
  printf ("}\n\n");
}

void
normalize_costs (struct transition_state *state)
{
  int i, delta = 0xfffffff;

  for (i = 0; i < state->rule.length (); i++)
    {
      if (state->rule[i] != -1 && delta > state->cost[i])
	delta = state->cost[i];

      if (delta == 0)
	break;
    }

  if (delta == 0)
    return;

  for (i = 0; i < state->rule.length (); i++)
    {
      if (state->rule[i] != -1)
	state->cost[i] = state->cost[i] - delta;
    }
}

void
closure (struct transition_state *state)
{
  bool changed;
  int i, cost;

  do {
    changed = false;
    for (i = 0; i < rules.length (); i++)
      {
	if (rules[i]->type == NT2NT
	    && state->rule[rules[i]->u.non_terminal] != -1)
	  {
	    cost = ((rules[i]->cost == -1) ? 0
			 : rules[i]->cost)
		+ state->cost[rules[i]->u.non_terminal];

	    if (state->rule[rules[i]->lhs_nt] == -1
		|| cost < state->cost[rules[i]->lhs_nt])
	      {
		state->rule[rules[i]->lhs_nt] = i;
		state->cost[rules[i]->lhs_nt] = cost;
		changed = true;
	      }
	  }
      }
  } while (changed == true);
}

void
compute_leaf_states (vec<struct transition_state *> *worklist)
{
  int i, j, idx;

  for (i = 0; i < terminals.length (); i++)
    {
      struct transition_state *state = (struct transition_state *)
		 xcalloc (1, sizeof (struct transition_state));

      state->rule.reserve_exact (non_terminals.length ());
      state->cost.reserve_exact (non_terminals.length ());
      state->nt.reserve_exact (non_terminals.length ());
      for (j = 0; j < non_terminals.length (); j++)
        {
	  state->rule.safe_insert (j, -1);
	  state->cost.safe_insert (j, -1);
          state->nt.safe_insert (j, j);
	}

      for (j = 0; j < rules.length (); j++)
	{
	  if (rules[j]->type == NT2T 
	      && rules[j]->u.terminal == i)
	    {
	      if (rules[j]->spec_idx == -1)
		{
		  state->rule[rules[j]->lhs_nt] = j;
		  state->cost[rules[j]->lhs_nt] = 0;
		}
	      else if (state->rule[rules[j]->lhs_nt] == -1
		       || rules[j]->cost
			  < state->cost[rules[j]->lhs_nt])
		{
		  state->rule[rules[j]->lhs_nt] = j;
	      	  state->cost[rules[j]->lhs_nt]
		    = rules[j]->cost;
		}
	    }
	}
      normalize_costs (state);
      closure (state);

      worklist->safe_push (state);
      idx = states.length ();
      state->id = idx;
      states.safe_insert (idx, state);
      terminals[i]->state = idx;
    }
}

struct transition_state *
project (enum primop_code pcode, int div, int sel, int type, int idx,
	 struct transition_state *state)
{
  vec<int> ntlist = vNULL;
  vec<int> costlist = vNULL;
  vec<int> rulelist = vNULL;
  int j;
 struct transition_state *new_state = (struct transition_state *)
		 xcalloc (1, sizeof (struct transition_state));

  for (j = 0; j < rules.length (); j++)
    {
      if (rules[j]->type != NT2OP)
	continue;

      if (rules[j]->u.rhs_exp.primop.op != pcode
	  || rules[j]->u.rhs_exp.primop.division != div
	  || non_terminals[rules[j]->lhs_nt]->type != type
	  || rules[j]->u.rhs_exp.primop.out_type != type)
	continue;

      if (rules[j]->u.rhs_exp.primop.op == POP_EXTR
	  && rules[j]->u.rhs_exp.primop.opd_selector != sel)
	continue;

//      if (state->cost[rules[j]->u.rhs_exp.rhs_nt[idx]] == -1)
//	continue;

      ntlist.safe_insert (ntlist.length (), rules[j]->u.rhs_exp.rhs_nt[idx]);
      costlist.safe_insert (costlist.length (),
	 state->cost[rules[j]->u.rhs_exp.rhs_nt[idx]]);
      rulelist.safe_insert (rulelist.length (), j);
    }

  new_state->rule = rulelist.copy ();
  new_state->cost = costlist.copy ();
  new_state->nt = ntlist.copy ();
  new_state->id = -1;

  normalize_costs (new_state);

  return new_state;
}
bool
increment_next (int idx, int arity, vec<int> *opd_list,
		vec<struct transition_state *> rep_states[],
		int pstate_idx, int pstate_loc)
{
  int i;
  
  if (idx == pstate_loc)
    {
      (*opd_list)[idx] = pstate_loc;
      idx++;
    }

  if (idx >= arity - 1)
    return false;

  (*opd_list)[idx] = 0;

  if ((*opd_list)[idx + 1] < rep_states[idx + 1].length () - 1)
    {
      (*opd_list)[idx + 1]++;
      return true;
    }

  return increment_next (idx + 1, arity, opd_list, rep_states,
		 pstate_idx, pstate_loc);
}

bool
rep_state_combination_next (struct transition_state *proj_state,
			    vec<struct transition_state *> rep_states[],
			    int act_arity, int idx, int rep_loc,
			    vec<int> *opd_list)
{
  int i, j;

  for (i = 0; i < act_arity; i++)
    if (rep_states[i] == vNULL)
      return false;


  if (*opd_list == vNULL)
    {
      for (i = 0; i < act_arity; i++)
 	if (i != idx)
          opd_list->safe_insert (i, 0);
	else
	  opd_list->safe_insert (i, rep_loc);
      return true;
    }

  for (i = 0; i < act_arity; i++)
    {
      if (i == idx)
	{
	  (*opd_list)[i] = rep_loc;
	  continue;
	}
      if ((*opd_list)[i] < rep_states[i].length () - 1)
	{
	  (*opd_list)[i]++;
	  return true;
	}
      else
	{
	  for (j = 0; j < i; j++)
	    if (i != idx)
	      (*opd_list)[j] = 0;
          return increment_next (j, act_arity, opd_list,
			 rep_states, idx, rep_loc);
	}
    }
  return false;
}

bool
is_state_equal_p (struct transition_state *st1, struct transition_state *st2)
{
  int i;

  if (st1->nt.length () != st2->nt.length ())
    return false;

  if (st1->rule.length () != st2->rule.length ())
    return false;

  if (st1->cost.length () != st2->cost.length ())
    return false;

  for (i = 0; i < st1->nt.length (); i++)
    {

      if (st1->nt[i] != st2->nt[i]
	  || st1->cost[i] != st2->cost[i])
	{
	  return false;
	}
    }
  return true;
}

void
trim_state_table ()
{
}

void
compute_transitions (struct operator_info *op,
		     struct transition_state *state,
		     vec<struct transition_state *> *worklist)
{
  int i, j, l;
  long long k;
  int cost;
  struct transition_state *proj_state;
  struct transition_state *result = (struct transition_state *)
		 xcalloc (1, sizeof (struct transition_state));

  result->rule.reserve_exact (non_terminals.length ());
  result->cost.reserve_exact (non_terminals.length ());
  result->nt.reserve_exact (non_terminals.length ());
  result->id = -1;
  for (j = 0; j < non_terminals.length (); j++)
    {
      result->rule.safe_insert (j, -1);
      result->cost.safe_insert (j, -1);
      result->nt.safe_insert (j, j);
    }

  for (i = 0; i < op->act_arity; i++)
    {
      vec<int> opd_list;
      proj_state = project (op->pcode, op->arity, op->sel, op->type, i, state);

      for (j = 0; j < op->rep_states[i].length (); j++)
        {
	  if (is_state_equal_p (proj_state, op->rep_states[i][j]))
	    break;
	}

      proj_state->id = j;
      if (j == op->rep_states[i].length ())
	{
	  op->rep_states[i].safe_insert (j, proj_state);
	}
      else
        continue;

      if (op->map[i].length () < state->id)
        op->map[i].safe_grow_cleared (state->id, NULL);
      op->map[i].safe_insert (state->id, proj_state);

      opd_list = vNULL;
      for (k = 0; k < op->rep_states[i].length (); k++)
	{
	  if (op->rep_states[i][k]->nt == vNULL)
	    break;
	}

      if (k != op->rep_states[i].length ())
	continue;

      while (rep_state_combination_next (proj_state,
					 op->rep_states,
					 op->act_arity,
					 i,
					 j,
					 &opd_list))
	{
      	  for(j = 0; j < rules.length (); j++)
	    {
	      if (rules[j]->type != NT2OP)
	        continue;

	      if (rules[j]->cost == -1)
		continue;

	      if (rules[j]->u.rhs_exp.primop.op != op->pcode)
	        continue;

	      if (rules[j]->u.rhs_exp.primop.division != op->arity)
	        continue;

	      if (rules[j]->u.rhs_exp.primop.opd_selector != op->sel)
	        continue;

	      if (rules[j]->u.rhs_exp.primop.out_type != op->type)
	        continue;


	      if (non_terminals[rules[j]->lhs_nt]->type != op->type)
		continue;

	      for (k = 0; k < proj_state->nt.length (); k++)
		if (proj_state->nt[k] == rules[j]->u.rhs_exp.rhs_nt[i])
		  break;

	      if (k == proj_state->nt.length () || proj_state->cost[k] == -1)
		continue;

	      cost = rules[j]->cost + proj_state->cost[k];
	      for (k = 0; k < op->act_arity; k++)
		{
		  if (k == i)
		    continue;

		  for (l = 0;
		       l < op->rep_states[k][opd_list[k]]->nt.length ();
		       l++)
		    if (op->rep_states[k][opd_list[k]]->nt[l]
			== rules[j]->u.rhs_exp.rhs_nt[k])
		      break;

		  if (l == op->rep_states[k][opd_list[k]]->nt.length () 
		      || op->rep_states[k][opd_list[k]]->cost[l] == -1)
		    {
		      cost = -1;
		      break;
		    }
		  else
		    cost = cost
			   + op->rep_states[k][opd_list[k]]->cost[l];
		}
	      if (cost == -1)
		continue;

	      if ((result->cost[rules[j]->lhs_nt] == -1
		      || cost < result->cost[rules[j]->lhs_nt]))
		{
	          result->cost.safe_insert (rules[j]->lhs_nt, cost);
		  result->rule.safe_insert (rules[j]->lhs_nt, j);
		  result->nt.safe_insert (rules[j]->lhs_nt, rules[j]->lhs_nt);
		}
	    }
	  normalize_costs (result);
	  closure (result);


	  for (k = 0; k < states.length (); k++)
	    if (is_state_equal_p (result, states[k]))
	      break;

	  result->id = k;
	  if (k == states.length ())
	    {
	      states.safe_insert (k, result);

	      worklist->safe_insert (worklist->length (), result);
	    }

	  long long int index = 0;
	  for (k = op->act_arity - 1; k >= 0; k--)
	    {
	      index = (index << 16) | opd_list[k];
	    }

	  for (k = 0; k < op->index_map.length (); k++)
	    if (index == op->index_map[k] /*&& is_state_equal_p (result,
			 states[op->trans_map[k]])*/)
	      	break;

	  if (k != op->index_map.length ())
	    continue;

	  k = op->index_map.length ();
	  op->index_map.safe_insert (k, index);
	  op->trans_map.safe_insert (k, result->id);
	  op->state_map.safe_insert (k, state->id);
	}
      
    }
}

vec<struct operator_info *> op_list = vNULL;
struct operator_info *
create_op (enum primop_code pcode, int arity, int sel, int act_arity, int type)
{
  struct operator_info *new_op;
  int i;

  new_op = (struct operator_info *) xcalloc (1, sizeof (struct operator_info));
  new_op->pcode = pcode;
  new_op->arity = arity;
  new_op->sel = sel;
  new_op->type = type;
  new_op->act_arity = act_arity;
  new_op->index_map = vNULL;
  new_op->trans_map = vNULL;
  for (i = 0; i < act_arity; i++)
    {
      new_op->map[i] = vNULL;
      new_op->rep_states[i] = vNULL;
    }

  return new_op;
}

void
create_transition_table ()
{
  vec<struct transition_state *> worklist;
  int i, j, k;
  struct operator_info *op; 

  states = vNULL;
  worklist = vNULL;
  compute_leaf_states (&worklist);
  for (k = 0; k < type_name.length (); k++)
    for (i = 0; i < arity_list.length (); i++)
      {
        for (j = 0; j < arity_list[i]; j++)
          {
     	    op = create_op (POP_EXTR, arity_list[i], j, 1, k);
	    op_list.safe_insert (op_list.length (), op);
          }

      	op = create_op (POP_ILV, arity_list[i], -1,
					 arity_list[i], k);
	op_list.safe_insert (op_list.length (), op);
    }

  while (worklist.length () != 0)
    {
      struct transition_state *state;
      state = worklist.pop ();

      for (i = 0; i < op_list.length (); i++)
	{
          compute_transitions (op_list[i], state, &worklist);
	}
    }
}

void
print_states ()
{
  int i, j;
  printf ("enum unif_vect_state {\n");
  for (i = 0; i < states.length (); i++)
    {
      printf ("  /*  state_%d: \n", i);
      for (j = 0; j < non_terminals.length (); j++)
	{
	  if (states[i]->rule[j] != -1)
	    {
	      printf ("\t");
	      print_grammar_rule (rules[states[i]->rule[j]]);
	      printf ("\t\t%d : %d >> %d\n", states[i]->nt[j], states[i]->cost[j], j);
	    }
	  /*else if (states[i]->cost[j] != -1)
	    {
	      printf ("\t\t%d : %d\n", states[i]->nt[j], states[i]->cost[j]);
	    }*/
	}
      printf ("  */\n");
      printf ("  UNIF_VECT_STATE_%d = %d,\n", i, i);
    }
  printf ("  UNIF_VECT_STATE_MAX = %d};\n\n", i);
  printf ("int state_nt_to_rule_map[UNIF_VECT_STATE_MAX][UNIF_VECT_NT_MAX];\n");

  printf ("void\ninit_state_to_rule_map ()\n{\n");
  printf ("memset (state_nt_to_rule_map, -1, %d);\n", states.length () * non_terminals.length ());
  for (i = 0; i < states.length (); i++)
    {
      for (j = 0; j < non_terminals.length (); j++)
        {
	  if (states[i]->rule[j] != -1)
	    {
	      printf ("state_nt_to_rule_map[UNIF_VECT_STATE_%d][UNIF_VECT_NT_%s] = %d;\n", i, non_terminals[j]->str, states[i]->rule[j]);
	    }
	}
    }
  printf ("}\n\n");
}

void
print_rep_state ()
{
  int i, j, k, l;
  printf ("struct {\n");
  for (i = 0; i < op_list.length (); i++)
    {

      for (j = 0; j < op_list[i]->act_arity; j++)
	{
          if (op_list[i]->pcode == POP_ILV)
	    {
      	      printf ("  vec<int> nt_%s_%s_%d_%d",
		 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity, j);
	      printf ("[%d];\n", op_list[i]->rep_states[j].length ());
      	      printf ("  vec<int> cost_%s_%s_%d_%d",
		 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity, j);
	      printf ("[%d];\n", op_list[i]->rep_states[j].length ());

	    }
      	  else
	    {
	      printf ("  vec<int> nt_%s_%s_%d_%d",
		 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity, op_list[i]->sel);
	      printf ("[%d];\n", op_list[i]->rep_states[j].length ());
	      printf ("  vec<int> cost_%s_%s_%d_%d",
		 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity, op_list[i]->sel);
	      printf ("[%d];\n", op_list[i]->rep_states[j].length ());

	    }


	}
    }
  printf ("} rep_state;\n\n");
  printf ("void\ninit_rep_states ()\n{\n");
  for (i = 0; i < op_list.length (); i++)
    {

      for (j = 0; j < op_list[i]->act_arity; j++)
	{
	  for (k = 0; k < op_list[i]->rep_states[j].length (); k++)
	    {
	      for (l = 0; l < op_list[i]->rep_states[j][k]->nt.length (); l++)
		{
	    	  if (op_list[i]->pcode == POP_ILV)
	    	    printf ("  rep_state.nt_%s_%s_%d_%d",
			 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
			 op_list[i]->arity, j);
    		  else
		    printf ("  rep_state.nt_%s_%s_%d_%d",
			 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
			 op_list[i]->arity, op_list[i]->sel);

	          printf ("[%d][%d] = UNIF_VECT_NT_%s;\n", k, l,
		      non_terminals[op_list[i]->rep_states[j][k]->nt[l]]->str);

	    	  if (op_list[i]->pcode == POP_ILV)
	    	    printf ("  rep_state.cost_%s_%s_%d_%d",
		        tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		        op_list[i]->arity, j);
    		  else
		    printf ("  rep_state.cost_%s_%s_%d_%d",
			tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
			op_list[i]->arity, op_list[i]->sel);

	          printf ("[%d][%d] = %d;\n", k, l,
		      op_list[i]->rep_states[j][k]->cost[l]);


		}
	    }
	}
    }

  printf ("}\n\n");

}

void
print_trans_map ()
{
  int i, j, k;
  long long index;
  char str[50], str1[100];
  printf ("struct {\n");
  for (j = 0; j < op_list.length (); j++)
    {
      if (op_list[j]->pcode == POP_ILV)
        printf ("  int %s_%s_%d", tree_code_name[op_list[j]->pcode], type_name[op_list[j]->type],
		 op_list[j]->arity);
      else
        printf ("  int %s_%s_%d_%d", tree_code_name[op_list[j]->pcode], type_name[op_list[j]->type],
		 op_list[j]->arity, op_list[j]->sel);

      for (k = 0; k < op_list[j]->act_arity; k++)
        {
      	  printf ("[1024]");
        }
      printf (";\n");
    }
  printf ("} transition;\n\n");
  printf ("int parent_state[%d][UNIF_VECT_STATE_MAX];\n\n", op_list.length ());
  printf ("void\ninit_transition_table ()\n{\n");
  for (j = 0; j < op_list.length (); j++)
    {
      if (op_list[j]->pcode == POP_ILV)
        sprintf (str, "  transition.%s_%s_%d", tree_code_name[op_list[j]->pcode], type_name[op_list[j]->type],
		 op_list[j]->arity);
      else
        sprintf (str, "  transition.%s_%s_%d_%d", tree_code_name[op_list[j]->pcode], type_name[op_list[j]->type],
		 op_list[j]->arity, op_list[j]->sel);

      for (i = 0; i < op_list[j]->index_map.length (); i++)
        {
          index = op_list[j]->index_map[i];
          sprintf (str1, "%s", str);
          for (k = 0; k < op_list[j]->act_arity; k++)
	    {
	      sprintf (str1, "%s[%lld]", str1, (index & 0x00000000000003ff));
	      index = index >> 16;
	    }
          //printf ("%s.safe_insert (%s.length (), UNIF_VECT_STATE_%d);\n", str1, str1, op_list[j]->trans_map[i]);
          printf ("%s = UNIF_VECT_STATE_%d;\n", str1, op_list[j]->trans_map[i]);
        }
    }

/*  for (j = 0; j < op_list.length (); j++)
    {
      for (i = 0; i < op_list[j]->index_map.length (); i++)
        {
          printf ("parent_state[%d][UNIF_VECT_STATE_%d] = UNIF_VECT_STATE_%d;\n", j, op_list[j]->trans_map[i], op_list[j]->state_map[i]);
        }
    }*/

  printf ("}\n\n");
}

void
print_non_terminals ()
{
  int i;
  printf ("enum unif_vect_nt {\n");
  for (i = 0; i < non_terminals.length (); i++)
    {
      printf ("  UNIF_VECT_NT_%s = %d,\n", non_terminals[i]->str, i);
    }
  printf ("  UNIF_VECT_NT_MAX = %d};\n\n", i);
  for (i = 0; i < non_terminals.length (); i++)
    {
      printf ("int get_%s_nonterminal_state ()\n{\n", non_terminals[i]->str);
      printf ("  return UNIF_VECT_NT_%s;\n", non_terminals[i]->str);
      printf ("}\n\n");
    }

}

void
print_terminals ()
{
  int i;
  printf ("enum unif_vect_t {\n");
  for (i = 0; i < terminals.length (); i++)
    {
      printf ("  UNIF_VECT_T_%s = %d,\n", terminals[i]->str, i);
    }
  printf ("  UNIF_VECT_T_MAX = %d};\n\n", i);

  for (i = 0; i < terminals.length (); i++)
    {
      printf ("int get_%s_terminal_state ()\n{\n", terminals[i]->str);
      printf ("  return %d;\n", terminals[i]->state);
      printf ("}\n\n");
    }

  printf ("int get_REG_terminal_state (int type)\n{\n");
  printf ("  switch (type)\n  {\n");
  for (i = 0; i < type_name.length (); i++)
    {
      printf ("case %smode: return get_REG_%s_terminal_state ();\n",type_name[i], type_name[i]);
    }
  printf ("    default:\n      gcc_assert (!\"Type not supported\");\n");
  printf ("  }\n}\n\n");

  printf ("int get_CONST_terminal_state (int type)\n{\n");
  printf ("  switch (type)\n  {\n");
  for (i = 0; i < type_name.length (); i++)
    {
      printf ("case %smode: return get_CONST_%s_terminal_state ();\n",type_name[i], type_name[i]);
    }
  printf ("    default:\n      gcc_assert (!\"Type not supported\");\n");
  printf ("  }\n}\n\n");


}

void
print_op_list ()
{
  int i;

  printf ("enum unif_vect_op_list {\n");
  for (i = 0; i < op_list.length (); i++)
      if (op_list[i]->pcode == POP_ILV)
        printf ("  UNIF_VECT_%s_%s_%d,\n", tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity);
      else
        printf ("  UNIF_VECT%s_%s_%d_%d,\n", tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		 op_list[i]->arity, op_list[i]->sel);
  printf ("  UNIF_VECT_OP_MAX};\n\n");


}

void
print_map ()
{
  int i, j, k;
  int max = 0;

  printf ("enum map_op_name {\n");
  for (i = 0; i < op_list.length (); i++)
    {
      for (j = 0; j < op_list[i]->act_arity; j++)
	{
	  printf ("MAP_OP_%s_%s_%d_%d,\n", tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
		  op_list[i]->arity,
		  op_list[i]->sel == -1 ? j : op_list[i]->sel);

	  if (max < op_list[i]->map[j].length ())
	    max = op_list[i]->map[j].length ();
	}
    }
  printf ("MAP_OP_NAME_MAX};\n\n");
  printf ("int rep_state_map[MAP_OP_NAME_MAX][%d];\n\n", max);

  printf ("void\ninit_rep_state_map ()\n{\n");
  for (i = 0; i < op_list.length (); i++)
    {
      for (j = 0; j < op_list[i]->act_arity; j++)
	{
	  for (k = 0; k < op_list[i]->map[j].length (); k++)
	    if (op_list[i]->map[j][k] != NULL)
	      printf ("rep_state_map[MAP_OP_%s_%s_%d_%d][UNIF_VECT_STATE_%d] = %d;\n",
		 tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],  op_list[i]->arity,
		 op_list[i]->sel == -1 ? j : op_list[i]->sel, k,
		 op_list[i]->map[j][k]->id);
	      //printf ("%d, %d, %d\n", i, j, k);

	}
    }
  printf ("}\n\n");
}

void
print_init_func ()
{
  long long int num;
  char *str, *str1;
  int i, j;

  printf ("void\nunif_vect_init_funct ()\n{\n");
  str1 = (char *) xcalloc (20, sizeof (char)); 
  str = (char *) xcalloc (40, sizeof (char)); 
  for (i = 0; i < op_list.length (); i++)
    {
      num = 1;
      if (op_list[i]->pcode == POP_ILV)
        {
          sprintf (str1, "%s_%s_%d", tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
	 	op_list[i]->arity);
        }
      else
        {
          sprintf (str1, "%s_%s_%d_%d", tree_code_name[op_list[i]->pcode], type_name[op_list[i]->type],
	 	op_list[i]->arity, op_list[i]->sel);
        }

      sprintf (str, "%s", str1);
      for (j = 0; j < op_list[i]->act_arity; j++)
	{
	  num = num * 1024;
	}
      printf ("  memset (transition.%s, -1, %d * sizeof (int));\n", str1, num);
      
    }
  printf ("  memset (rep_state_map, -1, sizeof (rep_state_map));\n");
  printf ("  init_transition_table ();\n");  
  printf ("  init_rep_state_map ();\n");
  printf ("  init_state_to_rule_map (); \n");  
  printf ("  init_grammar_rules ();\n");
  printf ("  init_nt_2_rule_map ();\n}\n\n");
}

void
print_state_fn_for_ilv ()
{
  int i, j;
  char *str = (char *) xcalloc (100, sizeof (char));
  printf ("int\ntransition_state_for_ilv");
  printf (" (int act_arity, vec<int> state_idx, int type)\n{\n");

  for (i = 0; i < op_list.length (); i++)
    {
      if (op_list[i]->pcode == POP_ILV)
	{
	  printf ("  if (act_arity == %d && type == %smode)\n    {\n", op_list[i]->arity, type_name[op_list[i]->type]);
	  sprintf (str, "");
	  for (j = 0; j < op_list[i]->act_arity; j++)
	    {
	        printf ("\tif (rep_state_map[MAP_OP_ILV_%s_%d_%d]",
		type_name[op_list[i]->type], op_list[i]->arity, j);
		printf ("[state_idx[%d]] == -1)\n", j);
		printf ("\t  return -1;\/\/get_REG_terminal_state ();\n");
		sprintf (str,
		  "%s\n\t[rep_state_map[MAP_OP_ILV_%s_%d_%d][state_idx[%d]]]",
		  str, type_name[op_list[i]->type], op_list[i]->arity, j, j);
	    }
	  printf ("      return transition.ILV_%s_%d%s", type_name[op_list[i]->type], op_list[i]->arity, str);
	  printf (";\n    }\n");
	}
    }
  printf ("}\n\n");

}

void
print_state_fn_for_extr ()
{
  int i;

  printf ("int\ntransition_state_for_extr");
  printf (" (int act_arity, int sel, int state_idx, int type)\n{\n");

  for (i = 0; i < op_list.length (); i++)
    {
      if (op_list[i]->pcode == POP_EXTR)
	{
	  printf ("  if (act_arity == %d && sel == %d && type == %smode)\n    {\n",
		op_list[i]->arity,
		op_list[i]->sel, type_name[op_list[i]->type]);
	  printf ("      if (rep_state_map[MAP_OP_EXTR_%s_%d_%d]",
		type_name[op_list[i]->type], op_list[i]->arity, op_list[i]->sel);
	  printf ("[state_idx] == -1)\n");
	  printf ("\treturn -1;\/\/get_REG_terminal_state ();\n");
	  printf ("      return transition.EXTR_%s_%d_%d\n", type_name[op_list[i]->type], op_list[i]->arity,
		 op_list[i]->sel);
	  printf ("\t  [rep_state_map[MAP_OP_EXTR_%s_%d_%d][state_idx]];\n    }\n", type_name[op_list[i]->type],
		 op_list[i]->arity, op_list[i]->sel);
	}
    }
  printf ("}\n\n");
}

void
print_permute_order_fn ()
{
  printf ("struct vec_perm_order_spec target_spec[] = TARGET_VEC_PERM_CONST_ORDER;\n");
  printf ("void\nprint_permute_order (int ruleno)\n{\n");
  printf ("      printf (\"\\n\");\n");
  printf ("  if (rules[ruleno]->spec_idx != -1)\n    {\n");
  printf ("      for (int i = 0; i < target_spec[rules[ruleno]->spec_idx].out_vec_size; i++)\n");
  printf ("\t{\n");
  printf ("\t  printf (\"%%d \", target_spec[rules[ruleno]->spec_idx].perm_order[i]);\n");
  printf ("\t}\n");
  printf ("      printf (\"\\n\");\n");
  printf ("    }\n");
  printf ("}\n\n");
}

void
print_get_rule_no_fn ()
{
  printf ("/* Each entry holds list of rules which result in the non-terminal.  */\n");
  printf ("vec<int> nt_2_rule_rel[UNIF_VECT_NT_MAX];\n\n");
  printf ("void\ninit_nt_2_rule_map ()\n{");
  printf ("  int i, j;\n");
  printf ("  for (i = 0; i < UNIF_VECT_NT_MAX; i++)\n    {\n");
  printf ("      nt_2_rule_rel[i] = vNULL;");
  printf ("    }\n");
  printf ("  for (i = 0; i < rules.length (); i++)\n    {\n");
  printf ("      nt_2_rule_rel[rules[i]->lhs].safe_insert (nt_2_rule_rel[rules[i]->lhs].length (), i);\n");
  printf ("    }\n");
  printf ("}\n\n");

  printf ("int\nget_rule_number (struct primop_tree *ptree, int nt)\n{\n");
  printf ("  return state_nt_to_rule_map[PT_AUX(ptree)][nt];");
  printf ("}\n\n");
}

void
print_get_child_nt_fn ()
{
  printf ("bool is_NT2T_rule (int ruleno)\n{\n");
  printf ("  return (rules[ruleno]->type == NT2T);\n");
  printf ("}\n\n");
  printf ("int\nget_child_nt (int state, int ruleno, int nt_idx)\n{\n");
  printf ("  if (rules[ruleno]->type == NT2OP)\n");
  printf ("    return rules[ruleno]->u.rhs_exp.opd[nt_idx];\n");
  printf ("  if (rules[ruleno]->type == NT2NT)\n    {\n");
  printf ("    return get_child_nt (state, state_nt_to_rule_map[state][rules[ruleno]->u.non_terminal], nt_idx);\n    }\n");
  printf ("  if (rules[ruleno]->type == NT2T)\n");
  printf ("    gcc_assert (0);\n");
  printf ("}\n\n");
}

void
print_transition_table ()
{
 print_op_list ();
  print_non_terminals ();
  print_terminals ();
  print_states (); 
 
  print_grammar_rules ();
  print_trans_map ();
  print_map ();
  print_get_rule_no_fn ();
  print_get_child_nt_fn ();
  print_init_func ();
  print_state_fn_for_ilv ();
  print_state_fn_for_extr ();
  print_permute_order_fn ();
}

int main (int argc, const char **argv)
{
  printf ("/* Generated automatically by the program `genvect-inst-tiles'\n\
from the macro TARGET_VEC_PERM_CONST_ORDER in target header file.  */\n\n");
printf ("#include \"config.h\"\n");
printf ("#include \"system.h\"\n");
printf ("#include \"coretypes.h\"\n");
printf ("#include \"backend.h\"\n");
printf ("#include \"tree.h\"\n");
printf ("#include \"gimple.h\"\n");
printf ("#include \"predict.h\"\n");
printf ("#include \"tree-pass.h\"\n");
printf ("#include \"ssa.h\"\n");
printf ("#include \"cgraph.h\"\n");
printf ("#include \"fold-const.h\"\n");
printf ("#include \"stor-layout.h\"\n");
printf ("#include \"gimple-iterator.h\"\n");
printf ("#include \"gimple-walk.h\"\n");
printf ("#include \"tree-ssa-loop-manip.h\"\n");
printf ("#include \"tree-cfg.h\"\n");
printf ("#include \"cfgloop.h\"\n");
printf ("#include \"tree-vectorizer.h\"\n");
printf ("#include \"tree-ssa-propagate.h\"\n");
printf ("#include \"dbgcnt.h\"\n");
printf ("#include \"tree-scalar-evolution.h\"\n");
printf ("#include \"tree-vect-unified.h\"\n");
printf ("#include \"tree-pretty-print.h\"\n");
printf ("#include \"gimple-pretty-print.h\"\n");
printf ("#include \"target.h\"\n");
printf ("#include \"rtl.h\"\n");
printf ("#include \"tm_p.h\"\n");
printf ("#include \"optabs-tree.h\"\n");
printf ("#include \"dumpfile.h\"\n");
printf ("#include \"alias.h\"\n");
printf ("#include \"tree-eh.h\"\n");
printf ("#include \"gimplify.h\"\n");
printf ("#include \"gimplify-me.h\"\n");
printf ("#include \"tree-ssa-loop-ivopts.h\"\n");
printf ("#include \"tree-ssa-loop.h\"\n");
printf ("#include \"expr.h\"\n");
printf ("#include \"builtins.h\"\n");
printf ("#include \"params.h\"\n");
printf ("#include \"pretty-print.h\"\n");
printf ("\n");
  create_instruction_tiles ();
  print_instruction_tiles ();
  create_grammar_rules ();
  print_grammar_rules_in_comment ();
  create_transition_table ();
  print_transition_table ();
}

