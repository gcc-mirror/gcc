/* Convert language-specific tree expression to rtl instructions,
   for GNU compiler.
   Copyright (C) 1988, 92-97, 1998, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "cp-tree.h"
#include "toplev.h"
#include "except.h"
#include "tm_p.h"

#if 0
static tree extract_aggr_init PARAMS ((tree, tree));
static tree extract_scalar_init PARAMS ((tree, tree));
#endif
static rtx cplus_expand_expr PARAMS ((tree, rtx, enum machine_mode,
				    enum expand_modifier));

/* Hook used by output_constant to expand language-specific
   constants.  */

tree
cplus_expand_constant (cst)
     tree cst;
{
  switch (TREE_CODE (cst))
    {
    case PTRMEM_CST:
      {
	tree type = TREE_TYPE (cst);
	tree member;
	tree offset;
      
	/* Find the member.  */
	member = PTRMEM_CST_MEMBER (cst);

	if (TREE_CODE (member) == FIELD_DECL) 
	  {
	    /* Find the offset for the field.  */
	    offset = convert (sizetype,
			      size_binop (EASY_DIV_EXPR,
					  DECL_FIELD_BITPOS (member),
					  size_int (BITS_PER_UNIT)));

	    /* We offset all pointer to data members by 1 so that we
	       can distinguish between a null pointer to data member
	       and the first data member of a structure.  */
	    offset = size_binop (PLUS_EXPR, offset, size_int (1));
	
	    cst = cp_convert (type, offset);
	  }
	else
	  {
	    tree delta;
	    tree idx;
	    tree pfn;
	    tree delta2;

	    expand_ptrmemfunc_cst (cst, &delta, &idx, &pfn, &delta2);

	    cst = build_ptrmemfunc1 (type, delta, idx,
				     pfn, delta2);
	  }
      }
      break;

    default:
      /* There's nothing to do.  */
      break;
    }

  return cst;
}

/* Hook used by expand_expr to expand language-specific tree codes.  */

static rtx
cplus_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  int ignore = target == const0_rtx;

  if (ignore)
    target = 0;

  /* No sense saving up arithmetic to be done
     if it's all in the wrong mode to form part of an address.
     And force_operand won't know whether to sign-extend or zero-extend.  */

  if (mode != Pmode && modifier == EXPAND_SUM)
    modifier = EXPAND_NORMAL;

  switch (code)
    {
    case PTRMEM_CST:
      return expand_expr (cplus_expand_constant (exp),
			  target, tmode, modifier);

    case OFFSET_REF:
      {
	return expand_expr (default_conversion (resolve_offset_ref (exp)),
			    target, tmode, EXPAND_NORMAL);
      }

    case THUNK_DECL:
      my_friendly_assert (DECL_RTL (exp) != NULL_RTX, 20000115);
      return DECL_RTL (exp);

    case THROW_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      expand_internal_throw ();
      return NULL;

    case EMPTY_CLASS_EXPR:
      /* We don't need to generate any code for an empty class.  */
      return const0_rtx;

    case STMT_EXPR:
      {
	tree rtl_expr = begin_stmt_expr ();
	expand_stmt (STMT_EXPR_STMT (exp));
	finish_stmt_expr (rtl_expr);
	return expand_expr (rtl_expr, target, tmode, modifier);
      }
      break;

    default:
      break;
    }
  my_friendly_abort (40);
  /* NOTREACHED */
  return NULL;
}

void
init_cplus_expand ()
{
  lang_expand_expr = cplus_expand_expr;
  lang_expand_constant = cplus_expand_constant;
}

/* If DECL had its rtl moved from where callers expect it
   to be, fix it up.  RESULT is the nominal rtl for the RESULT_DECL,
   which may be a pseudo instead of a hard register.  */

void
fixup_result_decl (decl, result)
     tree decl;
     rtx result;
{
  if (REG_P (result))
    {
      if (REGNO (result) >= FIRST_PSEUDO_REGISTER)
	{
	  rtx real_decl_result;

#ifdef FUNCTION_OUTGOING_VALUE
	  real_decl_result
	    = FUNCTION_OUTGOING_VALUE (TREE_TYPE (decl), current_function_decl);
#else
	  real_decl_result
	    = FUNCTION_VALUE (TREE_TYPE (decl), current_function_decl);
#endif
	  REG_FUNCTION_VALUE_P (real_decl_result) = 1;
	  result = real_decl_result;
	}
      store_expr (decl, result, 0);
      emit_insn (gen_rtx (USE, VOIDmode, result));
    }
}

#if 0
/* Expand this initialization inline and see if it's simple enough that
   it can be done at compile-time.  */

static tree
extract_aggr_init (decl, init)
     tree decl, init;
{
  return 0;
}

static tree
extract_scalar_init (decl, init)
     tree decl, init;
{
  rtx value, insns, insn;
  extern struct obstack temporary_obstack;
  tree t = NULL_TREE;

  start_sequence ();
  value = expand_expr (init, NULL_RTX, VOIDmode, 0);
  insns = get_insns ();
  end_sequence ();
  reg_scan (insns, max_reg_num (), 0);
  jump_optimize (insns, 0, 0, 1);

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      rtx r, to;

      if (GET_CODE (insn) == NOTE)
	continue;
      else if (GET_CODE (insn) != INSN)
	return 0;

      r = PATTERN (insn);
      if (GET_CODE (r) != SET)
	return 0;

      to = XEXP (r, 0);

      if (! (to == value
	     || (GET_CODE (to) == SUBREG && XEXP (to, 0) == value)))
	return 0;

      r = XEXP (r, 1);

      switch (GET_CODE (r))
	{
	case CONST_INT:
	  t = build_int_2 (XEXP (r, 0), 0);
	  break;
	default:
	  return 0;
	}
    }

  return t; 
}
#endif

int
extract_init (decl, init)
     tree decl ATTRIBUTE_UNUSED, init ATTRIBUTE_UNUSED;
{
  return 0;

#if 0
  if (IS_AGGR_TYPE (TREE_TYPE (decl))
      || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    init = extract_aggr_init (decl, init);
  else
    init = extract_scalar_init (decl, init);

  if (init == NULL_TREE)
    return 0;

  DECL_INITIAL (decl) = init;
  return 1;
#endif
}

void
do_case (start, end)
     tree start, end;
{
  tree value1 = NULL_TREE, value2 = NULL_TREE, label;

  if (start != NULL_TREE && TREE_TYPE (start) != NULL_TREE 
      && POINTER_TYPE_P (TREE_TYPE (start)))
    error ("pointers are not permitted as case values");

  if (end && pedantic)
    pedwarn ("ISO C++ forbids range expressions in switch statement");

  if (start)
    value1 = check_cp_case_value (start);
  if (end)
    value2 = check_cp_case_value (end);
  
  label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

  if (value1 != error_mark_node
      && value2 != error_mark_node)
    {
      tree duplicate;
      int success;

      if (end)
	success = pushcase_range (value1, value2, convert_and_check,
				  label, &duplicate);
      else if (start)
	success = pushcase (value1, convert_and_check, label, &duplicate);
      else
	success = pushcase (NULL_TREE, 0, label, &duplicate);

      if (success == 1)
	{
	  if (end)
	    error ("case label not within a switch statement");
	  else if (start)
	    cp_error ("case label `%E' not within a switch statement", start);
	  else
	    error ("default label not within a switch statement");
	}
      else if (success == 2)
	{
	  if (end)
	    {
	      error ("duplicate (or overlapping) case value");
	      cp_error_at ("this is the first entry overlapping that value",
			   duplicate);
	    }
	  else if (start)
	    {
	      cp_error ("duplicate case value `%E'", start);
	      cp_error_at ("previously used here", duplicate);
	    }
	  else
	    {
	      error ("multiple default labels in one switch");
	      cp_error_at ("this is the first default label", duplicate);
	    }
	}
      else if (success == 3)
	warning ("case value out of range");
      else if (success == 4)
	warning ("empty range specified");
      else if (success == 5)
	{
	  if (end)
	    error ("case label within scope of cleanup or variable array");
	  else if (! start)
	    error ("`default' label within scope of cleanup or variable array");
	  else
	    cp_error ("case label `%E' within scope of cleanup or variable array", start);
	}
    }

  current_function_return_value = NULL_TREE;
}
