/* IPA predicates.
   Copyright (C) 2003-2021 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "cgraph.h"
#include "tree-vrp.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "real.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "gimple.h"
#include "gimplify.h"
#include "data-streamer.h"


/* Check whether two set of operations have same effects.  */
static bool
expr_eval_ops_equal_p (expr_eval_ops ops1, expr_eval_ops ops2)
{
  if (ops1)
    {
      if (!ops2 || ops1->length () != ops2->length ())
	return false;

      for (unsigned i = 0; i < ops1->length (); i++)
	{
	  expr_eval_op &op1 = (*ops1)[i];
	  expr_eval_op &op2 = (*ops2)[i];

	  if (op1.code != op2.code
	      || op1.index != op2.index
	      || !vrp_operand_equal_p (op1.val[0], op2.val[0])
	      || !vrp_operand_equal_p (op1.val[1], op2.val[1])
	      || !types_compatible_p (op1.type, op2.type))
	    return false;
	}
      return true;
    }
  return !ops2;
}

/* Add clause CLAUSE into the predicate P.
   When CONDITIONS is NULL do not perform checking whether NEW_CLAUSE
   is obviously true.  This is useful only when NEW_CLAUSE is known to be
   sane.  */

void
predicate::add_clause (conditions conditions, clause_t new_clause)
{
  int i;
  int i2;
  int insert_here = -1;
  int c1, c2;

  /* True clause.  */
  if (!new_clause)
    return;

  /* False clause makes the whole predicate false.  Kill the other variants.  */
  if (new_clause == (1 << predicate::false_condition))
    {
      *this = false;
      return;
    }
  if (*this == false)
    return;

  /* No one should be silly enough to add false into nontrivial clauses.  */
  gcc_checking_assert (!(new_clause & (1 << predicate::false_condition)));

  /* Look where to insert the new_clause.  At the same time prune out
     new_clauses of P that are implied by the new new_clause and thus
     redundant.  */
  for (i = 0, i2 = 0; i <= max_clauses; i++)
    {
      m_clause[i2] = m_clause[i];

      if (!m_clause[i])
	break;

      /* If m_clause[i] implies new_clause, there is nothing to add.  */
      if ((m_clause[i] & new_clause) == m_clause[i])
	{
	  /* We had nothing to add, none of clauses should've become
	     redundant.  */
	  gcc_checking_assert (i == i2);
	  return;
	}

      if (m_clause[i] < new_clause && insert_here < 0)
	insert_here = i2;

      /* If new_clause implies clause[i], then clause[i] becomes redundant.
         Otherwise the clause[i] has to stay.  */
      if ((m_clause[i] & new_clause) != new_clause)
	i2++;
    }

  /* Look for clauses that are obviously true.  I.e.
     op0 == 5 || op0 != 5.  */
  if (conditions)
    for (c1 = predicate::first_dynamic_condition;
	 c1 < num_conditions; c1++)
      {
	condition *cc1;
	if (!(new_clause & (1 << c1)))
	  continue;
	cc1 = &(*conditions)[c1 - predicate::first_dynamic_condition];
	/* We have no way to represent !changed and !is_not_constant
	   and thus there is no point for looking for them.  */
	if (cc1->code == changed || cc1->code == is_not_constant)
	  continue;
	for (c2 = c1 + 1; c2 < num_conditions; c2++)
	  if (new_clause & (1 << c2))
	    {
	      condition *cc2 =
		&(*conditions)[c2 - predicate::first_dynamic_condition];
	      if (cc1->operand_num == cc2->operand_num
		  && vrp_operand_equal_p (cc1->val, cc2->val)
		  && cc2->code != is_not_constant
		  && cc2->code != changed
		  && expr_eval_ops_equal_p (cc1->param_ops, cc2->param_ops)
		  && cc2->agg_contents == cc1->agg_contents
		  && cc2->by_ref == cc1->by_ref
		  && types_compatible_p (cc2->type, cc1->type)
		  && cc1->code == invert_tree_comparison (cc2->code,
							  HONOR_NANS (cc1->val)))
		return;
	    }
      }


  /* We run out of variants.  Be conservative in positive direction.  */
  if (i2 == max_clauses)
    return;
  /* Keep clauses in decreasing order. This makes equivalence testing easy.  */
  m_clause[i2 + 1] = 0;
  if (insert_here >= 0)
    for (; i2 > insert_here; i2--)
      m_clause[i2] = m_clause[i2 - 1];
  else
    insert_here = i2;
  m_clause[insert_here] = new_clause;
}


/* Do THIS &= P.  */

predicate &
predicate::operator &= (const predicate &p)
{
  /* Avoid busy work.  */
  if (p == false || *this == true)
    {
      *this = p;
      return *this;
    }
  if (*this == false || p == true || this == &p)
    return *this;

  int i;

  /* See how far predicates match.  */
  for (i = 0; m_clause[i] && m_clause[i] == p.m_clause[i]; i++)
    {
      gcc_checking_assert (i < max_clauses);
    }

  /* Combine the predicates rest.  */
  for (; p.m_clause[i]; i++)
    {
      gcc_checking_assert (i < max_clauses);
      add_clause (NULL, p.m_clause[i]);
    }
  return *this;
}



/* Return THIS | P2.  */

predicate
predicate::or_with (conditions conditions,
	            const predicate &p) const
{
  /* Avoid busy work.  */
  if (p == false || *this == true || *this == p)
    return *this;
  if (*this == false || p == true)
    return p;

  /* OK, combine the predicates.  */
  predicate out = true;

  for (int i = 0; m_clause[i]; i++)
    for (int j = 0; p.m_clause[j]; j++)
      {
	gcc_checking_assert (i < max_clauses && j < max_clauses);
	out.add_clause (conditions, m_clause[i] | p.m_clause[j]);
      }
  return out;
}


/* Having partial truth assignment in POSSIBLE_TRUTHS, return false
   if predicate P is known to be false.  */

bool
predicate::evaluate (clause_t possible_truths) const
{
  int i;

  /* True remains true.  */
  if (*this == true)
    return true;

  gcc_assert (!(possible_truths & (1 << predicate::false_condition)));

  /* See if we can find clause we can disprove.  */
  for (i = 0; m_clause[i]; i++)
    {
      gcc_checking_assert (i < max_clauses);
      if (!(m_clause[i] & possible_truths))
	return false;
    }
  return true;
}

/* Return the probability in range 0...REG_BR_PROB_BASE that the predicated
   instruction will be recomputed per invocation of the inlined call.  */

int
predicate::probability (conditions conds,
	                clause_t possible_truths,
	                vec<inline_param_summary> inline_param_summary) const
{
  int i;
  int combined_prob = REG_BR_PROB_BASE;

  /* True remains true.  */
  if (*this == true)
    return REG_BR_PROB_BASE;

  if (*this == false)
    return 0;

  gcc_assert (!(possible_truths & (1 << predicate::false_condition)));

  /* See if we can find clause we can disprove.  */
  for (i = 0; m_clause[i]; i++)
    {
      gcc_checking_assert (i < max_clauses);
      if (!(m_clause[i] & possible_truths))
	return 0;
      else
	{
	  int this_prob = 0;
	  int i2;
	  if (!inline_param_summary.exists ())
	    return REG_BR_PROB_BASE;
	  for (i2 = 0; i2 < num_conditions; i2++)
	    if ((m_clause[i] & possible_truths) & (1 << i2))
	      {
		if (i2 >= predicate::first_dynamic_condition)
		  {
		    condition *c =
		      &(*conds)[i2 - predicate::first_dynamic_condition];
		    if (c->code == predicate::changed
			&& (c->operand_num <
			    (int) inline_param_summary.length ()))
		      {
			int iprob =
			  inline_param_summary[c->operand_num].change_prob;
			this_prob = MAX (this_prob, iprob);
		      }
		    else
		      this_prob = REG_BR_PROB_BASE;
		  }
		else
		  this_prob = REG_BR_PROB_BASE;
	      }
	  combined_prob = MIN (this_prob, combined_prob);
	  if (!combined_prob)
	    return 0;
	}
    }
  return combined_prob;
}


/* Dump conditional COND.  */

void
dump_condition (FILE *f, conditions conditions, int cond)
{
  condition *c;
  if (cond == predicate::false_condition)
    fprintf (f, "false");
  else if (cond == predicate::not_inlined_condition)
    fprintf (f, "not inlined");
  else
    {
      c = &(*conditions)[cond - predicate::first_dynamic_condition];
      fprintf (f, "op%i", c->operand_num);
      if (c->agg_contents)
	fprintf (f, "[%soffset: " HOST_WIDE_INT_PRINT_DEC "]",
		 c->by_ref ? "ref " : "", c->offset);

      for (unsigned i = 0; i < vec_safe_length (c->param_ops); i++)
	{
	  expr_eval_op &op = (*(c->param_ops))[i];
	  const char *op_name = op_symbol_code (op.code);

	  if (op_name == op_symbol_code (ERROR_MARK))
	    op_name = get_tree_code_name (op.code);

	  fprintf (f, ",(");

	  if (!op.val[0])
	    {
	      switch (op.code)
		{
		case FLOAT_EXPR:
		case FIX_TRUNC_EXPR:
		case FIXED_CONVERT_EXPR:
		case VIEW_CONVERT_EXPR:
		CASE_CONVERT:
		  if (op.code == VIEW_CONVERT_EXPR)
		    fprintf (f, "VCE");
		  fprintf (f, "(");
		  print_generic_expr (f, op.type);
		  fprintf (f, ")" );
		  break;

		default:
		  fprintf (f, "%s", op_name);
		}
	      fprintf (f, " #");
	    }
	  else if (!op.val[1])
	    {
	      if (op.index)
		{
		  print_generic_expr (f, op.val[0]);
		  fprintf (f, " %s #", op_name);
		}
	      else
		{
		  fprintf (f, "# %s ", op_name);
		  print_generic_expr (f, op.val[0]);
		}
	    }
	  else
	    {
	      fprintf (f, "%s ", op_name);
	      switch (op.index)
		{
		case 0:
		  fprintf (f, "#, ");
		  print_generic_expr (f, op.val[0]);
		  fprintf (f, ", ");
		  print_generic_expr (f, op.val[1]);
		  break;

		case 1:
		  print_generic_expr (f, op.val[0]);
		  fprintf (f, ", #, ");
		  print_generic_expr (f, op.val[1]);
		  break;

		case 2:
		  print_generic_expr (f, op.val[0]);
		  fprintf (f, ", ");
		  print_generic_expr (f, op.val[1]);
		  fprintf (f, ", #");
		  break;

		default:
		  fprintf (f, "*, *, *");
		}
	    }
	  fprintf (f, ")");
	}

      if (c->code == predicate::is_not_constant)
	{
	  fprintf (f, " not constant");
	  return;
	}
      if (c->code == predicate::changed)
	{
	  fprintf (f, " changed");
	  return;
	}
      fprintf (f, " %s ", op_symbol_code (c->code));
      print_generic_expr (f, c->val);
    }
}


/* Dump clause CLAUSE.  */

static void
dump_clause (FILE *f, conditions conds, clause_t clause)
{
  int i;
  bool found = false;
  fprintf (f, "(");
  if (!clause)
    fprintf (f, "true");
  for (i = 0; i < predicate::num_conditions; i++)
    if (clause & (1 << i))
      {
	if (found)
	  fprintf (f, " || ");
	found = true;
	dump_condition (f, conds, i);
      }
  fprintf (f, ")");
}


/* Dump THIS to F.  CONDS a vector of conditions used when evaluating
   predicates.  When NL is true new line is output at the end of dump.  */

void
predicate::dump (FILE *f, conditions conds, bool nl) const
{
  int i;
  if (*this == true)
    dump_clause (f, conds, 0);
  else
    for (i = 0; m_clause[i]; i++)
      {
	if (i)
	  fprintf (f, " && ");
	dump_clause (f, conds, m_clause[i]);
      }
  if (nl)
    fprintf (f, "\n");
}


void
predicate::debug (conditions conds) const
{
  dump (stderr, conds);
}


/* Remap predicate THIS of former function to be predicate of duplicated function.
   POSSIBLE_TRUTHS is clause of possible truths in the duplicated node,
   INFO is inline summary of the duplicated node.  */

predicate
predicate::remap_after_duplication (clause_t possible_truths)
{
  int j;
  predicate out = true;
  for (j = 0; m_clause[j]; j++)
    if (!(possible_truths & m_clause[j]))
      return false;
    else
      out.add_clause (NULL, possible_truths & m_clause[j]);
  return out;
}


/* Translate all conditions from callee representation into caller
   representation and symbolically evaluate predicate THIS into new predicate.

   INFO is ipa_fn_summary of function we are adding predicate into, CALLEE_INFO
   is summary of function predicate P is from. OPERAND_MAP is array giving
   callee formal IDs the caller formal IDs. POSSSIBLE_TRUTHS is clause of all
   callee conditions that may be true in caller context.  TOPLEV_PREDICATE is
   predicate under which callee is executed.  OFFSET_MAP is an array of
   offsets that need to be added to conditions, negative offset means that
   conditions relying on values passed by reference have to be discarded
   because they might not be preserved (and should be considered offset zero
   for other purposes).  */

predicate
predicate::remap_after_inlining (class ipa_fn_summary *info,
				 class ipa_node_params *params_summary,
				 class ipa_fn_summary *callee_info,
				 const vec<int> &operand_map,
				 const vec<HOST_WIDE_INT> &offset_map,
				 clause_t possible_truths,
				 const predicate &toplev_predicate)
{
  int i;
  predicate out = true;

  /* True predicate is easy.  */
  if (*this == true)
    return toplev_predicate;
  for (i = 0; m_clause[i]; i++)
    {
      clause_t clause = m_clause[i];
      int cond;
      predicate clause_predicate = false;

      gcc_assert (i < max_clauses);

      for (cond = 0; cond < num_conditions; cond++)
	/* Do we have condition we can't disprove?   */
	if (clause & possible_truths & (1 << cond))
	  {
	    predicate cond_predicate;
	    /* Work out if the condition can translate to predicate in the
	       inlined function.  */
	    if (cond >= predicate::first_dynamic_condition)
	      {
		struct condition *c;

		c = &(*callee_info->conds)[cond
					   -
					   predicate::first_dynamic_condition];
		/* See if we can remap condition operand to caller's operand.
		   Otherwise give up.  */
		if (!operand_map.exists ()
		    || (int) operand_map.length () <= c->operand_num
		    || operand_map[c->operand_num] == -1
		    /* TODO: For non-aggregate conditions, adding an offset is
		       basically an arithmetic jump function processing which
		       we should support in future.  */
		    || ((!c->agg_contents || !c->by_ref)
			&& offset_map[c->operand_num] > 0)
		    || (c->agg_contents && c->by_ref
			&& offset_map[c->operand_num] < 0))
		  cond_predicate = true;
		else
		  {
		    struct agg_position_info ap;
		    HOST_WIDE_INT offset_delta = offset_map[c->operand_num];
		    if (offset_delta < 0)
		      {
			gcc_checking_assert (!c->agg_contents || !c->by_ref);
			offset_delta = 0;
		      }
		    gcc_assert (!c->agg_contents
				|| c->by_ref || offset_delta == 0);
		    ap.offset = c->offset + offset_delta;
		    ap.agg_contents = c->agg_contents;
		    ap.by_ref = c->by_ref;
		    cond_predicate = add_condition (info, params_summary,
						    operand_map[c->operand_num],
						    c->type, &ap, c->code,
						    c->val, c->param_ops);
		  }
	      }
	    /* Fixed conditions remains same, construct single
	       condition predicate.  */
	    else
	      cond_predicate = predicate::predicate_testing_cond (cond);
	    clause_predicate = clause_predicate.or_with (info->conds,
					                 cond_predicate);
	  }
      out &= clause_predicate;
    }
  out &= toplev_predicate;
  return out;
}


/* Read predicate from IB.  */

void
predicate::stream_in (class lto_input_block *ib)
{
  clause_t clause;
  int k = 0;

  do
    {
      gcc_assert (k <= max_clauses);
      clause = m_clause[k++] = streamer_read_uhwi (ib);
    }
  while (clause);

  /* Zero-initialize the remaining clauses in OUT.  */
  while (k <= max_clauses)
    m_clause[k++] = 0;
}


/* Write predicate P to OB.  */

void
predicate::stream_out (struct output_block *ob)
{
  int j;
  for (j = 0; m_clause[j]; j++)
    {
      gcc_assert (j < max_clauses);
      streamer_write_uhwi (ob, m_clause[j]);
    }
  streamer_write_uhwi (ob, 0);
}


/* Add condition to condition list SUMMARY.  OPERAND_NUM, TYPE, CODE, VAL and
   PARAM_OPS correspond to fields of condition structure.  AGGPOS describes
   whether the used operand is loaded from an aggregate and where in the
   aggregate it is.  It can be NULL, which means this not a load from an
   aggregate.  */

predicate
add_condition (class ipa_fn_summary *summary,
	       class ipa_node_params *params_summary,
	       int operand_num,
	       tree type, struct agg_position_info *aggpos,
	       enum tree_code code, tree val, expr_eval_ops param_ops)
{
  int i, j;
  struct condition *c;
  struct condition new_cond;
  HOST_WIDE_INT offset;
  bool agg_contents, by_ref;
  expr_eval_op *op;

  if (params_summary)
    ipa_set_param_used_by_ipa_predicates (params_summary, operand_num, true);

  if (aggpos)
    {
      offset = aggpos->offset;
      agg_contents = aggpos->agg_contents;
      by_ref = aggpos->by_ref;
    }
  else
    {
      offset = 0;
      agg_contents = false;
      by_ref = false;
    }

  gcc_checking_assert (operand_num >= 0);
  for (i = 0; vec_safe_iterate (summary->conds, i, &c); i++)
    {
      if (c->operand_num == operand_num
	  && c->code == code
	  && types_compatible_p (c->type, type)
	  && vrp_operand_equal_p (c->val, val)
	  && c->agg_contents == agg_contents
	  && expr_eval_ops_equal_p (c->param_ops, param_ops)
	  && (!agg_contents || (c->offset == offset && c->by_ref == by_ref)))
	return predicate::predicate_testing_cond (i);
    }
  /* Too many conditions.  Give up and return constant true.  */
  if (i == predicate::num_conditions - predicate::first_dynamic_condition)
    return true;

  new_cond.operand_num = operand_num;
  new_cond.code = code;
  new_cond.type = unshare_expr_without_location (type);
  new_cond.val = val ? unshare_expr_without_location (val) : val;
  new_cond.agg_contents = agg_contents;
  new_cond.by_ref = by_ref;
  new_cond.offset = offset;
  new_cond.param_ops = vec_safe_copy (param_ops);

  for (j = 0; vec_safe_iterate (new_cond.param_ops, j, &op); j++)
    {
      if (op->val[0])
	op->val[0] = unshare_expr_without_location (op->val[0]);
      if (op->val[1])
	op->val[1] = unshare_expr_without_location (op->val[1]);
    }

  vec_safe_push (summary->conds, new_cond);

  return predicate::predicate_testing_cond (i);
}
