/* Pretty formatting of GIMPLE statements and expressions.
   Copyright (C) 2001-2024 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Diego Novillo <dnovillo@google.com>

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "dumpfile.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-predict.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "value-range-pretty-print.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "dumpfile.h"	/* for dump_flags */
#include "value-prof.h"
#include "trans-mem.h"
#include "cfganal.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "cfgloop.h"
#include "gimple-range.h"

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

#define INDENT(SPACE)							\
  do { int i; for (i = 0; i < SPACE; i++) pp_space (pp); } while (0)

#define GIMPLE_NIY do_niy (pp,gs)

/* Try to print on PP a default message for the unrecognized
   gimple statement GS.  */

static void
do_niy (pretty_printer *pp, const gimple *gs)
{
  pp_printf (pp, "<<< Unknown GIMPLE statement: %s >>>\n",
	     gimple_code_name[(int) gimple_code (gs)]);
}


/* Emit a newline and SPC indentation spaces to PP.  */

static void
newline_and_indent (pretty_printer *pp, int spc)
{
  pp_newline (pp);
  INDENT (spc);
}


/* Print the GIMPLE statement GS on stderr.  */

DEBUG_FUNCTION void
debug_gimple_stmt (gimple *gs)
{
  print_gimple_stmt (stderr, gs, 0, TDF_VOPS|TDF_MEMSYMS);
}


/* Return formatted string of a VALUE probability
   (biased by REG_BR_PROB_BASE).  Returned string is allocated
   by xstrdup_for_dump.  */

static const char *
dump_profile (profile_count &count)
{
  char *buf = NULL;
  if (!count.initialized_p ())
    return "";
  if (count.ipa_p ())
    buf = xasprintf ("[count: %" PRId64 "]",
		     count.to_gcov_type ());
  else if (count.initialized_p ())
    buf = xasprintf ("[local count: %" PRId64 "]",
		     count.to_gcov_type ());

  const char *ret = xstrdup_for_dump (buf);
  free (buf);

  return ret;
}

/* Return formatted string of a VALUE probability
   (biased by REG_BR_PROB_BASE).  Returned string is allocated
   by xstrdup_for_dump.  */

static const char *
dump_probability (profile_probability probability)
{
  float minimum = 0.01f;
  float fvalue = -1;

  if (probability.initialized_p ())
    {
      fvalue = probability.to_reg_br_prob_base () * 100.0f / REG_BR_PROB_BASE;
      if (fvalue < minimum && probability.to_reg_br_prob_base ())
	fvalue = minimum;
    }

  char *buf;
  if (probability.initialized_p ())
    buf = xasprintf ("[%.2f%%]", fvalue);
  else
    buf = xasprintf ("[INV]");

  const char *ret = xstrdup_for_dump (buf);
  free (buf);

  return ret;
}

/* Dump E probability to PP.  */

static void
dump_edge_probability (pretty_printer *pp, edge e)
{
  pp_scalar (pp, " %s", dump_probability (e->probability));
}

/* Print GIMPLE statement G to FILE using SPC indentation spaces and
   FLAGS as in pp_gimple_stmt_1.  */

void
print_gimple_stmt (FILE *file, gimple *g, int spc, dump_flags_t flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  pp_gimple_stmt_1 (&pp, g, spc, flags);
  pp_newline_and_flush (&pp);
}

DEBUG_FUNCTION void
debug (gimple &ref)
{
  print_gimple_stmt (stderr, &ref, 0, TDF_NONE);
}

DEBUG_FUNCTION void
debug (gimple *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Print GIMPLE statement G to FILE using SPC indentation spaces and
   FLAGS as in pp_gimple_stmt_1.  Print only the right-hand side
   of the statement.  */

void
print_gimple_expr (FILE *file, gimple *g, int spc, dump_flags_t flags)
{
  flags |= TDF_RHS_ONLY;
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  pp_gimple_stmt_1 (&pp, g, spc, flags);
  pp_flush (&pp);
}


/* Print the GIMPLE sequence SEQ on PP using SPC indentation
   spaces and FLAGS as in pp_gimple_stmt_1.
   The caller is responsible for calling pp_flush on PP to finalize
   the pretty printer.  */

static void
dump_gimple_seq (pretty_printer *pp, gimple_seq seq, int spc,
		 dump_flags_t flags)
{
  gimple_stmt_iterator i;

  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gimple *gs = gsi_stmt (i);
      INDENT (spc);
      pp_gimple_stmt_1 (pp, gs, spc, flags);
      if (!gsi_one_before_end_p (i))
	pp_newline (pp);
    }
}


/* Print GIMPLE sequence SEQ to FILE using SPC indentation spaces and
   FLAGS as in pp_gimple_stmt_1.  */

void
print_gimple_seq (FILE *file, gimple_seq seq, int spc, dump_flags_t flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  dump_gimple_seq (&pp, seq, spc, flags);
  pp_newline_and_flush (&pp);
}


/* Print the GIMPLE sequence SEQ on stderr.  */

DEBUG_FUNCTION void
debug_gimple_seq (gimple_seq seq)
{
  print_gimple_seq (stderr, seq, 0, TDF_VOPS|TDF_MEMSYMS);
}


/* A simple helper to pretty-print some of the gimple tuples in the printf
   style. The format modifiers are preceded by '%' and are:
     'G' - outputs a string corresponding to the code of the given gimple,
     'S' - outputs a gimple_seq with indent of spc + 2,
     'T' - outputs the tree t,
     'd' - outputs an int as a decimal,
     's' - outputs a string,
     'n' - outputs a newline,
     'x' - outputs an int as hexadecimal,
     '+' - increases indent by 2 then outputs a newline,
     '-' - decreases indent by 2 then outputs a newline.   */

static void
dump_gimple_fmt (pretty_printer *pp, int spc, dump_flags_t flags,
                 const char *fmt, ...)
{
  va_list args;
  const char *c;
  const char *tmp;

  va_start (args, fmt);
  for (c = fmt; *c; c++)
    {
      if (*c == '%')
        {
          gimple_seq seq;
          tree t;
	  gimple *g;
          switch (*++c)
            {
              case 'G':
		g = va_arg (args, gimple *);
                tmp = gimple_code_name[gimple_code (g)];
                pp_string (pp, tmp);
                break;

              case 'S':
                seq = va_arg (args, gimple_seq);
                pp_newline (pp);
                dump_gimple_seq (pp, seq, spc + 2, flags);
                newline_and_indent (pp, spc);
                break;

              case 'T':
                t = va_arg (args, tree);
                if (t == NULL_TREE)
                  pp_string (pp, "NULL");
                else
                  dump_generic_node (pp, t, spc, flags, false);
                break;

              case 'd':
                pp_decimal_int (pp, va_arg (args, int));
                break;

              case 's':
                pp_string (pp, va_arg (args, char *));
                break;

              case 'n':
                newline_and_indent (pp, spc);
                break;

	      case 'x':
		pp_scalar (pp, "%x", va_arg (args, int));
		break;

              case '+':
                spc += 2;
                newline_and_indent (pp, spc);
                break;

              case '-':
                spc -= 2;
                newline_and_indent (pp, spc);
                break;

              default:
                gcc_unreachable ();
            }
        }
      else
        pp_character (pp, *c);
    }
  va_end (args);
}


/* Helper for dump_gimple_assign.  Print the unary RHS of the
   assignment GS.  PP, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_unary_rhs (pretty_printer *pp, const gassign *gs, int spc,
		dump_flags_t flags)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (gs);
  tree lhs = gimple_assign_lhs (gs);
  tree rhs = gimple_assign_rhs1 (gs);

  switch (rhs_code)
    {
    case VIEW_CONVERT_EXPR:
      dump_generic_node (pp, rhs, spc, flags, false);
      break;

    case FIXED_CONVERT_EXPR:
    case ADDR_SPACE_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
      pp_left_paren (pp);
      dump_generic_node (pp, TREE_TYPE (lhs), spc, flags, false);
      pp_string (pp, ") ");
      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, rhs, spc, flags, false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, rhs, spc, flags, false);
      break;

    case PAREN_EXPR:
      pp_string (pp, "((");
      dump_generic_node (pp, rhs, spc, flags, false);
      pp_string (pp, "))");
      break;

    case ABS_EXPR:
    case ABSU_EXPR:
      if (flags & TDF_GIMPLE)
	{
	  pp_string (pp,
		     rhs_code == ABS_EXPR ? "__ABS " : "__ABSU ");
	  dump_generic_node (pp, rhs, spc, flags, false);
	}
      else
	{
	  pp_string (pp,
		     rhs_code == ABS_EXPR ? "ABS_EXPR <" : "ABSU_EXPR <");
	  dump_generic_node (pp, rhs, spc, flags, false);
	  pp_greater (pp);
	}
      break;

    default:
      if (TREE_CODE_CLASS (rhs_code) == tcc_declaration
	  || TREE_CODE_CLASS (rhs_code) == tcc_constant
	  || TREE_CODE_CLASS (rhs_code) == tcc_reference
	  || rhs_code == SSA_NAME
	  || rhs_code == ADDR_EXPR
	  || rhs_code == CONSTRUCTOR)
	{
	  dump_generic_node (pp, rhs, spc, flags, false);
	  break;
	}
      else if (rhs_code == BIT_NOT_EXPR)
	pp_complement (pp);
      else if (rhs_code == TRUTH_NOT_EXPR)
	pp_exclamation (pp);
      else if (rhs_code == NEGATE_EXPR)
	pp_minus (pp);
      else
	{
	  pp_left_bracket (pp);
	  pp_string (pp, get_tree_code_name (rhs_code));
	  pp_string (pp, "] ");
	}

      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, rhs, spc, flags, false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, rhs, spc, flags, false);
      break;
    }
}


/* Helper for dump_gimple_assign.  Print the binary RHS of the
   assignment GS.  PP, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_binary_rhs (pretty_printer *pp, const gassign *gs, int spc,
		 dump_flags_t flags)
{
  const char *p;
  enum tree_code code = gimple_assign_rhs_code (gs);
  switch (code)
    {
    case MIN_EXPR:
    case MAX_EXPR:
      if (flags & TDF_GIMPLE)
	{
	  pp_string (pp, code == MIN_EXPR ? "__MIN (" : "__MAX (");
	  dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags,
			     false);
	  pp_string (pp, ", ");
	  dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags,
			     false);
	  pp_string (pp, ")");
	  break;
	}
      else
	{
	  gcc_fallthrough ();
	}
    case COMPLEX_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_PACK_FLOAT_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
    case VEC_SERIES_EXPR:
      for (p = get_tree_code_name (code); *p; p++)
	pp_character (pp, TOUPPER (*p));
      pp_string (pp, " <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    default:
      if (op_prio (gimple_assign_rhs1 (gs)) <= op_code_prio (code))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags,
			     false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_space (pp);
      pp_string (pp, op_symbol_code (gimple_assign_rhs_code (gs), flags));
      pp_space (pp);
      if (op_prio (gimple_assign_rhs2 (gs)) <= op_code_prio (code))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags,
			     false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
    }
}

/* Helper for dump_gimple_assign.  Print the ternary RHS of the
   assignment GS.  PP, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_ternary_rhs (pretty_printer *pp, const gassign *gs, int spc,
		  dump_flags_t flags)
{
  const char *p;
  enum tree_code code = gimple_assign_rhs_code (gs);
  switch (code)
    {
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
      for (p = get_tree_code_name (code); *p; p++)
	pp_character (pp, TOUPPER (*p));
      pp_string (pp, " <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    case DOT_PROD_EXPR:
      pp_string (pp, "DOT_PROD_EXPR <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    case SAD_EXPR:
      pp_string (pp, "SAD_EXPR <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    case VEC_PERM_EXPR:
      if (flags & TDF_GIMPLE)
	pp_string (pp, "__VEC_PERM (");
      else
	pp_string (pp, "VEC_PERM_EXPR <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      if (flags & TDF_GIMPLE)
	pp_right_paren (pp);
      else
	pp_greater (pp);
      break;

    case REALIGN_LOAD_EXPR:
      pp_string (pp, "REALIGN_LOAD <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    case COND_EXPR:
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, " ? ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, " : ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      break;

    case VEC_COND_EXPR:
      pp_string (pp, "VEC_COND_EXPR <");
      dump_generic_node (pp, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (pp);
      break;

    case BIT_INSERT_EXPR:
      if (flags & TDF_GIMPLE)
	{
	  pp_string (pp, "__BIT_INSERT (");
	  dump_generic_node (pp, gimple_assign_rhs1 (gs), spc,
			     flags | TDF_SLIM, false);
	  pp_string (pp, ", ");
	  dump_generic_node (pp, gimple_assign_rhs2 (gs), spc,
			     flags | TDF_SLIM, false);
	  pp_string (pp, ", ");
	  dump_generic_node (pp, gimple_assign_rhs3 (gs), spc,
			     flags | TDF_SLIM, false);
	  pp_right_paren (pp);
	}
      else
	{
	  pp_string (pp, "BIT_INSERT_EXPR <");
	  dump_generic_node (pp, gimple_assign_rhs1 (gs),
			     spc, flags, false);
	  pp_string (pp, ", ");
	  dump_generic_node (pp, gimple_assign_rhs2 (gs),
			     spc, flags, false);
	  pp_string (pp, ", ");
	  dump_generic_node (pp, gimple_assign_rhs3 (gs),
			     spc, flags, false);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs2 (gs))))
	    {
	      pp_string (pp, " (");
	      pp_decimal_int (pp, TYPE_PRECISION
			      (TREE_TYPE (gimple_assign_rhs2 (gs))));
	      pp_string (pp, " bits)");
	    }
	  pp_greater (pp);
	}
      break;

    default:
      gcc_unreachable ();
    }
}


/* Dump the gimple assignment GS.  PP, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_assign (pretty_printer *pp, const gassign *gs, int spc,
		    dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      tree arg1 = NULL;
      tree arg2 = NULL;
      tree arg3 = NULL;
      switch (gimple_num_ops (gs))
	{
	case 4:
	  arg3 = gimple_assign_rhs3 (gs);
	  /* FALLTHRU */
	case 3:
	  arg2 = gimple_assign_rhs2 (gs);
	  /* FALLTHRU */
	case 2:
	  arg1 = gimple_assign_rhs1 (gs);
	  break;
	default:
	  gcc_unreachable ();
	}

      dump_gimple_fmt (pp, spc, flags, "%G <%s, %T, %T, %T, %T>", gs,
		       get_tree_code_name (gimple_assign_rhs_code (gs)),
                       gimple_assign_lhs (gs), arg1, arg2, arg3);
    }
  else
    {
      if (!(flags & TDF_RHS_ONLY))
	{
	  dump_generic_node (pp, gimple_assign_lhs (gs), spc, flags, false);
	  pp_space (pp);
	  pp_equal (pp);

	  if (gimple_assign_nontemporal_move_p (gs))
	    pp_string (pp, "{nt}");

	  if (gimple_has_volatile_ops (gs))
	    pp_string (pp, "{v}");

	  pp_space (pp);
	}

      if (gimple_num_ops (gs) == 2)
	dump_unary_rhs (pp, gs, spc,
			((flags & TDF_GIMPLE)
			 && gimple_assign_rhs_class (gs) != GIMPLE_SINGLE_RHS)
			? (flags | TDF_GIMPLE_VAL) : flags);
      else if (gimple_num_ops (gs) == 3)
	dump_binary_rhs (pp, gs, spc,
			 (flags & TDF_GIMPLE)
			 ? (flags | TDF_GIMPLE_VAL) : flags);
      else if (gimple_num_ops (gs) == 4)
	dump_ternary_rhs (pp, gs, spc,
			  (flags & TDF_GIMPLE)
			  ? (flags | TDF_GIMPLE_VAL) : flags);
      else
        gcc_unreachable ();
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon (pp);
    }
}


/* Dump the return statement GS.  PP, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_return (pretty_printer *pp, const greturn *gs, int spc,
		    dump_flags_t flags)
{
  tree t;

  t = gimple_return_retval (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T>", gs, t);
  else
    {
      pp_string (pp, "return");
      if (t)
	{
	  pp_space (pp);
	  dump_generic_node (pp, t, spc, flags, false);
	}
      pp_semicolon (pp);
    }
}


/* Dump the call arguments for a gimple call. PP, FLAGS are as in
   dump_gimple_call.  */

static void
dump_gimple_call_args (pretty_printer *pp, const gcall *gs,
		       dump_flags_t flags)
{
  size_t i = 0;

  /* Pretty print first arg to certain internal fns.  */
  if (gimple_call_internal_p (gs))
    {
      const char *const *enums = NULL;
      unsigned limit = 0;

      switch (gimple_call_internal_fn (gs))
	{
	case IFN_UNIQUE:
#define DEF(X) #X
	  static const char *const unique_args[] = {IFN_UNIQUE_CODES};
#undef DEF
	  enums = unique_args;

	  limit = ARRAY_SIZE (unique_args);
	  break;

	case IFN_GOACC_LOOP:
#define DEF(X) #X
	  static const char *const loop_args[] = {IFN_GOACC_LOOP_CODES};
#undef DEF
	  enums = loop_args;
	  limit = ARRAY_SIZE (loop_args);
	  break;

	case IFN_GOACC_REDUCTION:
#define DEF(X) #X
	  static const char *const reduction_args[]
	    = {IFN_GOACC_REDUCTION_CODES};
#undef DEF
	  enums = reduction_args;
	  limit = ARRAY_SIZE (reduction_args);
	  break;

	case IFN_HWASAN_MARK:
	case IFN_ASAN_MARK:
#define DEF(X) #X
	  static const char *const asan_mark_args[] = {IFN_ASAN_MARK_FLAGS};
#undef DEF
	  enums = asan_mark_args;
	  limit = ARRAY_SIZE (asan_mark_args);
	  break;

	default:
	  break;
	}
      if (limit)
	{
	  tree arg0 = gimple_call_arg (gs, 0);
	  HOST_WIDE_INT v;

	  if (TREE_CODE (arg0) == INTEGER_CST
	      && tree_fits_shwi_p (arg0)
	      && (v = tree_to_shwi (arg0)) >= 0 && v < limit)
	    {
	      i++;
	      pp_string (pp, enums[v]);
	    }
	}
    }

  for (; i < gimple_call_num_args (gs); i++)
    {
      if (i)
	pp_string (pp, ", ");
      dump_generic_node (pp, gimple_call_arg (gs, i), 0, flags, false);
    }

  if (gimple_call_va_arg_pack_p (gs))
    {
      if (i)
	pp_string (pp, ", ");

      pp_string (pp, "__builtin_va_arg_pack ()");
    }
}

/* Dump the points-to solution *PT to PP.  */

static void
pp_points_to_solution (pretty_printer *pp, const pt_solution *pt)
{
  if (pt->anything)
    {
      pp_string (pp, "anything ");
      return;
    }
  if (pt->nonlocal)
    pp_string (pp, "nonlocal ");
  if (pt->escaped)
    pp_string (pp, "escaped ");
  if (pt->ipa_escaped)
    pp_string (pp, "unit-escaped ");
  if (pt->null)
    pp_string (pp, "null ");
  if (pt->const_pool)
    pp_string (pp, "const-pool ");
  if (pt->vars
      && !bitmap_empty_p (pt->vars))
    {
      bitmap_iterator bi;
      unsigned i;
      pp_string (pp, "{ ");
      EXECUTE_IF_SET_IN_BITMAP (pt->vars, 0, i, bi)
	{
	  pp_string (pp, "D.");
	  pp_decimal_int (pp, i);
	  pp_space (pp);
	}
      pp_right_brace (pp);
      if (pt->vars_contains_nonlocal
	  || pt->vars_contains_escaped
	  || pt->vars_contains_escaped_heap
	  || pt->vars_contains_restrict
	  || pt->vars_contains_interposable)
	{
	  const char *comma = "";
	  pp_string (pp, " (");
	  if (pt->vars_contains_nonlocal)
	    {
	      pp_string (pp, "nonlocal");
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped)
	    {
	      pp_string (pp, comma);
	      pp_string (pp, "escaped");
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped_heap)
	    {
	      pp_string (pp, comma);
	      pp_string (pp, "escaped heap");
	      comma = ", ";
	    }
	  if (pt->vars_contains_restrict)
	    {
	      pp_string (pp, comma);
	      pp_string (pp, "restrict");
	      comma = ", ";
	    }
	  if (pt->vars_contains_interposable)
	    {
	      pp_string (pp, comma);
	      pp_string (pp, "interposable");
	    }
	  pp_string (pp, ")");
	}

    }
}

/* Dump the call statement GS.  PP, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_call (pretty_printer *pp, const gcall *gs, int spc,
		  dump_flags_t flags)
{
  tree lhs = gimple_call_lhs (gs);
  tree fn = gimple_call_fn (gs);

  if (flags & TDF_ALIAS)
    {
      const pt_solution *pt;
      pt = gimple_call_use_set (gs);
      if (!pt_solution_empty_p (pt))
	{
	  pp_string (pp, "# USE = ");
	  pp_points_to_solution (pp, pt);
	  newline_and_indent (pp, spc);
	}
      pt = gimple_call_clobber_set (gs);
      if (!pt_solution_empty_p (pt))
	{
	  pp_string (pp, "# CLB = ");
	  pp_points_to_solution (pp, pt);
	  newline_and_indent (pp, spc);
	}
    }

  if (flags & TDF_RAW)
    {
      if (gimple_call_internal_p (gs))
	dump_gimple_fmt (pp, spc, flags, "%G <.%s, %T", gs,
			 internal_fn_name (gimple_call_internal_fn (gs)), lhs);
      else
	dump_gimple_fmt (pp, spc, flags, "%G <%T, %T", gs, fn, lhs);
      if (gimple_call_num_args (gs) > 0)
        {
          pp_string (pp, ", ");
          dump_gimple_call_args (pp, gs, flags);
        }
      pp_greater (pp);
    }
  else
    {
      if (lhs && !(flags & TDF_RHS_ONLY))
        {
          dump_generic_node (pp, lhs, spc, flags, false);
          pp_string (pp, " =");

	  if (gimple_has_volatile_ops (gs))
	    pp_string (pp, "{v}");

	  pp_space (pp);
        }
      if (gimple_call_internal_p (gs))
	{
	  pp_dot (pp);
	  pp_string (pp, internal_fn_name (gimple_call_internal_fn (gs)));
	}
      else
	print_call_name (pp, fn, flags);
      pp_string (pp, " (");
      dump_gimple_call_args (pp, gs, flags);
      pp_right_paren (pp);
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon (pp);
    }

  if (gimple_call_chain (gs))
    {
      pp_string (pp, " [static-chain: ");
      dump_generic_node (pp, gimple_call_chain (gs), spc, flags, false);
      pp_right_bracket (pp);
    }

  if (gimple_call_return_slot_opt_p (gs))
    pp_string (pp, " [return slot optimization]");
  if (gimple_call_tail_p (gs))
    pp_string (pp, " [tail call]");
  if (gimple_call_must_tail_p (gs))
    pp_string (pp, " [must tail call]");

  if (fn == NULL)
    return;

  /* Dump the arguments of _ITM_beginTransaction sanely.  */
  if (TREE_CODE (fn) == ADDR_EXPR)
    fn = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fn) == FUNCTION_DECL && decl_is_tm_clone (fn))
    pp_string (pp, " [tm-clone]");
  if (TREE_CODE (fn) == FUNCTION_DECL
      && fndecl_built_in_p (fn, BUILT_IN_TM_START)
      && gimple_call_num_args (gs) > 0)
    {
      tree t = gimple_call_arg (gs, 0);
      unsigned HOST_WIDE_INT props;
      gcc_assert (TREE_CODE (t) == INTEGER_CST);

      pp_string (pp, " [ ");

      /* Get the transaction code properties.  */
      props = TREE_INT_CST_LOW (t);

      if (props & PR_INSTRUMENTEDCODE)
	pp_string (pp, "instrumentedCode ");
      if (props & PR_UNINSTRUMENTEDCODE)
	pp_string (pp, "uninstrumentedCode ");
      if (props & PR_HASNOXMMUPDATE)
	pp_string (pp, "hasNoXMMUpdate ");
      if (props & PR_HASNOABORT)
	pp_string (pp, "hasNoAbort ");
      if (props & PR_HASNOIRREVOCABLE)
	pp_string (pp, "hasNoIrrevocable ");
      if (props & PR_DOESGOIRREVOCABLE)
	pp_string (pp, "doesGoIrrevocable ");
      if (props & PR_HASNOSIMPLEREADS)
	pp_string (pp, "hasNoSimpleReads ");
      if (props & PR_AWBARRIERSOMITTED)
	pp_string (pp, "awBarriersOmitted ");
      if (props & PR_RARBARRIERSOMITTED)
	pp_string (pp, "RaRBarriersOmitted ");
      if (props & PR_UNDOLOGCODE)
	pp_string (pp, "undoLogCode ");
      if (props & PR_PREFERUNINSTRUMENTED)
	pp_string (pp, "preferUninstrumented ");
      if (props & PR_EXCEPTIONBLOCK)
	pp_string (pp, "exceptionBlock ");
      if (props & PR_HASELSE)
	pp_string (pp, "hasElse ");
      if (props & PR_READONLY)
	pp_string (pp, "readOnly ");

      pp_right_bracket (pp);
    }
}


/* Dump the switch statement GS.  PP, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_switch (pretty_printer *pp, const gswitch *gs, int spc,
		    dump_flags_t flags)
{
  unsigned int i;

  GIMPLE_CHECK (gs, GIMPLE_SWITCH);
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T, ", gs,
                   gimple_switch_index (gs));
  else
    {
      pp_string (pp, "switch (");
      dump_generic_node (pp, gimple_switch_index (gs), spc, flags, true);
      if (flags & TDF_GIMPLE)
	pp_string (pp, ") {");
      else
	pp_string (pp, ") <");
    }

  for (i = 0; i < gimple_switch_num_labels (gs); i++)
    {
      tree case_label = gimple_switch_label (gs, i);
      gcc_checking_assert (case_label != NULL_TREE);
      dump_generic_node (pp, case_label, spc, flags, false);
      pp_space (pp);
      tree label = CASE_LABEL (case_label);
      dump_generic_node (pp, label, spc, flags, false);

      if (cfun && cfun->cfg)
	{
	  basic_block dest = label_to_block (cfun, label);
	  if (dest)
	    {
	      edge label_edge = find_edge (gimple_bb (gs), dest);
	      if (label_edge && !(flags & TDF_GIMPLE))
		dump_edge_probability (pp, label_edge);
	    }
	}

      if (i < gimple_switch_num_labels (gs) - 1)
	{
	  if (flags & TDF_GIMPLE)
	    pp_string (pp, "; ");
	  else
	    pp_string (pp, ", ");
	}
    }
  if (flags & TDF_GIMPLE)
    pp_string (pp, "; }");
  else
    pp_greater (pp);
}


/* Dump the gimple conditional GS.  PP, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_cond (pretty_printer *pp, const gcond *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%s, %T, %T, %T, %T>", gs,
		     get_tree_code_name (gimple_cond_code (gs)),
		     gimple_cond_lhs (gs), gimple_cond_rhs (gs),
		     gimple_cond_true_label (gs), gimple_cond_false_label (gs));
  else
    {
      if (!(flags & TDF_RHS_ONLY))
	pp_string (pp, "if (");
      dump_generic_node (pp, gimple_cond_lhs (gs), spc,
			 flags | ((flags & TDF_GIMPLE) ? TDF_GIMPLE_VAL : TDF_NONE),
			 false);
      pp_space (pp);
      pp_string (pp, op_symbol_code (gimple_cond_code (gs), flags));
      pp_space (pp);
      dump_generic_node (pp, gimple_cond_rhs (gs), spc,
			 flags | ((flags & TDF_GIMPLE) ? TDF_GIMPLE_VAL : TDF_NONE),
			 false);
      if (!(flags & TDF_RHS_ONLY))
	{
	  edge_iterator ei;
	  edge e, true_edge = NULL, false_edge = NULL;
	  basic_block bb = gimple_bb (gs);

	  if (bb)
	    {
	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  if (e->flags & EDGE_TRUE_VALUE)
		    true_edge = e;
		  else if (e->flags & EDGE_FALSE_VALUE)
		    false_edge = e;
		}
	    }

	  bool has_edge_info = true_edge != NULL && false_edge != NULL;

	  pp_right_paren (pp);

	  if (gimple_cond_true_label (gs))
	    {
	      pp_string (pp, " goto ");
	      dump_generic_node (pp, gimple_cond_true_label (gs),
				 spc, flags, false);
	      if (has_edge_info && !(flags & TDF_GIMPLE))
		dump_edge_probability (pp, true_edge);
	      pp_semicolon (pp);
	    }
	  if (gimple_cond_false_label (gs))
	    {
	      pp_string (pp, " else goto ");
	      dump_generic_node (pp, gimple_cond_false_label (gs),
				 spc, flags, false);
	      if (has_edge_info && !(flags & TDF_GIMPLE))
		dump_edge_probability (pp, false_edge);

	      pp_semicolon (pp);
	    }
	}
    }
}


/* Dump a GIMPLE_LABEL tuple on the pretty_printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfils.h).  */

static void
dump_gimple_label (pretty_printer *pp, const glabel *gs, int spc,
		   dump_flags_t flags)
{
  tree label = gimple_label_label (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T>", gs, label);
  else
    {
      dump_generic_node (pp, label, spc, flags, false);
      pp_colon (pp);
    }
  if (flags & TDF_GIMPLE)
    return;
  if (DECL_NONLOCAL (label))
    pp_string (pp, " [non-local]");
  if ((flags & TDF_EH) && EH_LANDING_PAD_NR (label))
    pp_printf (pp, " [LP %d]", EH_LANDING_PAD_NR (label));
}

/* Dump a GIMPLE_GOTO tuple on the pretty_printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  */

static void
dump_gimple_goto (pretty_printer *pp, const ggoto *gs, int spc,
		  dump_flags_t flags)
{
  tree label = gimple_goto_dest (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T>", gs, label);
  else
    dump_gimple_fmt (pp, spc, flags, "goto %T;", label);
}


/* Dump a GIMPLE_BIND tuple on the pretty_printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  */

static void
dump_gimple_bind (pretty_printer *pp, const gbind *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <", gs);
  else
    pp_left_brace (pp);
  if (!(flags & TDF_SLIM))
    {
      tree var;

      for (var = gimple_bind_vars (gs); var; var = DECL_CHAIN (var))
	{
          newline_and_indent (pp, 2);
	  print_declaration (pp, var, spc, flags);
	}
      if (gimple_bind_vars (gs))
	pp_newline (pp);
    }
  pp_newline (pp);
  dump_gimple_seq (pp, gimple_bind_body (gs), spc + 2, flags);
  newline_and_indent (pp, spc);
  if (flags & TDF_RAW)
    pp_greater (pp);
  else
    pp_right_brace (pp);
}


/* Dump a GIMPLE_TRY tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_try (pretty_printer *pp, const gtry *gs, int spc,
		 dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      const char *type;
      if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
        type = "GIMPLE_TRY_CATCH";
      else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
        type = "GIMPLE_TRY_FINALLY";
      else
        type = "UNKNOWN GIMPLE_TRY";
      dump_gimple_fmt (pp, spc, flags,
                       "%G <%s,%+EVAL <%S>%nCLEANUP <%S>%->", gs, type,
                       gimple_try_eval (gs), gimple_try_cleanup (gs));
    }
  else
    {
      pp_string (pp, "try");
      newline_and_indent (pp, spc + 2);
      pp_left_brace (pp);
      pp_newline (pp);

      dump_gimple_seq (pp, gimple_try_eval (gs), spc + 4, flags);
      newline_and_indent (pp, spc + 2);
      pp_right_brace (pp);

      gimple_seq seq = gimple_try_cleanup (gs);

      if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
	{
	  newline_and_indent (pp, spc);
	  pp_string (pp, "catch");
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	}
      else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
	{
	  newline_and_indent (pp, spc);
	  pp_string (pp, "finally");
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);

	  if (seq && is_a <geh_else *> (gimple_seq_first_stmt (seq))
	      && gimple_seq_nondebug_singleton_p (seq))
	    {
	      geh_else *stmt = as_a <geh_else *> (gimple_seq_first_stmt (seq));
	      seq = gimple_eh_else_n_body (stmt);
	      pp_newline (pp);
	      dump_gimple_seq (pp, seq, spc + 4, flags);
	      newline_and_indent (pp, spc + 2);
	      pp_right_brace (pp);
	      seq = gimple_eh_else_e_body (stmt);
	      newline_and_indent (pp, spc);
	      pp_string (pp, "else");
	      newline_and_indent (pp, spc + 2);
	      pp_left_brace (pp);
	    }
	}
      else
	pp_string (pp, " <UNKNOWN GIMPLE_TRY> {");

      pp_newline (pp);
      dump_gimple_seq (pp, seq, spc + 4, flags);
      newline_and_indent (pp, spc + 2);
      pp_right_brace (pp);
    }
}


/* Dump a GIMPLE_CATCH tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_catch (pretty_printer *pp, const gcatch *gs, int spc,
		   dump_flags_t flags)
{
  if (flags & TDF_RAW)
      dump_gimple_fmt (pp, spc, flags, "%G <%T, %+CATCH <%S>%->", gs,
                       gimple_catch_types (gs), gimple_catch_handler (gs));
  else
      dump_gimple_fmt (pp, spc, flags, "catch (%T)%+{%S}",
                       gimple_catch_types (gs), gimple_catch_handler (gs));
}


/* Dump a GIMPLE_EH_FILTER tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_eh_filter (pretty_printer *pp, const geh_filter *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T, %+FAILURE <%S>%->", gs,
                     gimple_eh_filter_types (gs),
                     gimple_eh_filter_failure (gs));
  else
    dump_gimple_fmt (pp, spc, flags, "<<<eh_filter (%T)>>>%+{%+%S%-}",
                     gimple_eh_filter_types (gs),
                     gimple_eh_filter_failure (gs));
}


/* Dump a GIMPLE_EH_MUST_NOT_THROW tuple.  */

static void
dump_gimple_eh_must_not_throw (pretty_printer *pp,
			       const geh_mnt *gs, int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T>", gs,
		     gimple_eh_must_not_throw_fndecl (gs));
  else
    dump_gimple_fmt (pp, spc, flags, "<<<eh_must_not_throw (%T)>>>",
		     gimple_eh_must_not_throw_fndecl (gs));
}


/* Dump a GIMPLE_EH_ELSE tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_eh_else (pretty_printer *pp, const geh_else *gs, int spc,
		     dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags,
		     "%G <%+N_BODY <%S>%nE_BODY <%S>%->", gs,
		     gimple_eh_else_n_body (gs), gimple_eh_else_e_body (gs));
  else
    dump_gimple_fmt (pp, spc, flags,
		    "<<<if_normal_exit>>>%+{%S}%-<<<else_eh_exit>>>%+{%S}",
		     gimple_eh_else_n_body (gs), gimple_eh_else_e_body (gs));
}


/* Dump a GIMPLE_RESX tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_resx (pretty_printer *pp, const gresx *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%d>", gs,
		     gimple_resx_region (gs));
  else
    dump_gimple_fmt (pp, spc, flags, "resx %d", gimple_resx_region (gs));
}

/* Dump a GIMPLE_EH_DISPATCH tuple on the pretty_printer PP.  */

static void
dump_gimple_eh_dispatch (pretty_printer *pp, const geh_dispatch *gs,
			 int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%d>", gs,
		     gimple_eh_dispatch_region (gs));
  else
    dump_gimple_fmt (pp, spc, flags, "eh_dispatch %d",
		     gimple_eh_dispatch_region (gs));
}

/* Dump a GIMPLE_DEBUG tuple on the pretty_printer PP, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_debug (pretty_printer *pp, const gdebug *gs, int spc,
		   dump_flags_t flags)
{
  switch (gs->subcode)
    {
    case GIMPLE_DEBUG_BIND:
      if (flags & TDF_RAW)
	dump_gimple_fmt (pp, spc, flags, "%G BIND <%T, %T>", gs,
			 gimple_debug_bind_get_var (gs),
			 gimple_debug_bind_get_value (gs));
      else
	dump_gimple_fmt (pp, spc, flags, "# DEBUG %T => %T",
			 gimple_debug_bind_get_var (gs),
			 gimple_debug_bind_get_value (gs));
      break;

    case GIMPLE_DEBUG_SOURCE_BIND:
      if (flags & TDF_RAW)
	dump_gimple_fmt (pp, spc, flags, "%G SRCBIND <%T, %T>", gs,
			 gimple_debug_source_bind_get_var (gs),
			 gimple_debug_source_bind_get_value (gs));
      else
	dump_gimple_fmt (pp, spc, flags, "# DEBUG %T s=> %T",
			 gimple_debug_source_bind_get_var (gs),
			 gimple_debug_source_bind_get_value (gs));
      break;

    case GIMPLE_DEBUG_BEGIN_STMT:
      if (flags & TDF_RAW)
	dump_gimple_fmt (pp, spc, flags, "%G BEGIN_STMT", gs);
      else
	dump_gimple_fmt (pp, spc, flags, "# DEBUG BEGIN_STMT");
      break;

    case GIMPLE_DEBUG_INLINE_ENTRY:
      if (flags & TDF_RAW)
	dump_gimple_fmt (pp, spc, flags, "%G INLINE_ENTRY %T", gs,
			 gimple_block (gs)
			 ? block_ultimate_origin (gimple_block (gs))
			 : NULL_TREE);
      else
	dump_gimple_fmt (pp, spc, flags, "# DEBUG INLINE_ENTRY %T",
			 gimple_block (gs)
			 ? block_ultimate_origin (gimple_block (gs))
			 : NULL_TREE);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Dump a GIMPLE_OMP_FOR tuple on the pretty_printer PP.  */
static void
dump_gimple_omp_for (pretty_printer *pp, const gomp_for *gs, int spc,
		     dump_flags_t flags)
{
  size_t i;

  if (flags & TDF_RAW)
    {
      const char *kind;
      switch (gimple_omp_for_kind (gs))
	{
	case GF_OMP_FOR_KIND_FOR:
	  kind = "";
	  break;
	case GF_OMP_FOR_KIND_DISTRIBUTE:
	  kind = " distribute";
	  break;
	case GF_OMP_FOR_KIND_TASKLOOP:
	  kind = " taskloop";
	  break;
	case GF_OMP_FOR_KIND_OACC_LOOP:
	  kind = " oacc_loop";
	  break;
	case GF_OMP_FOR_KIND_SIMD:
	  kind = " simd";
	  break;
	default:
	  gcc_unreachable ();
	}
      dump_gimple_fmt (pp, spc, flags, "%G%s <%+BODY <%S>%nCLAUSES <", gs,
		       kind, gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_for_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >,");
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	dump_gimple_fmt (pp, spc, flags,
			 "%+%T, %T, %T, %s, %T,%n",
			 gimple_omp_for_index (gs, i),
			 gimple_omp_for_initial (gs, i),
			 gimple_omp_for_final (gs, i),
			 get_tree_code_name (gimple_omp_for_cond (gs, i)),
			 gimple_omp_for_incr (gs, i));
      dump_gimple_fmt (pp, spc, flags, "PRE_BODY <%S>%->",
		       gimple_omp_for_pre_body (gs));
    }
  else
    {
      switch (gimple_omp_for_kind (gs))
	{
	case GF_OMP_FOR_KIND_FOR:
	  pp_string (pp, "#pragma omp for");
	  break;
	case GF_OMP_FOR_KIND_DISTRIBUTE:
	  pp_string (pp, "#pragma omp distribute");
	  break;
	case GF_OMP_FOR_KIND_TASKLOOP:
	  pp_string (pp, "#pragma omp taskloop");
	  break;
	case GF_OMP_FOR_KIND_OACC_LOOP:
	  pp_string (pp, "#pragma acc loop");
	  break;
	case GF_OMP_FOR_KIND_SIMD:
	  pp_string (pp, "#pragma omp simd");
	  break;
	default:
	  gcc_unreachable ();
	}
      dump_omp_clauses (pp, gimple_omp_for_clauses (gs), spc, flags);
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	{
	  if (i)
	    spc += 2;
	  newline_and_indent (pp, spc);
	  pp_string (pp, "for (");
	  dump_generic_node (pp, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_string (pp, " = ");
	  tree init = gimple_omp_for_initial (gs, i);
	  if (TREE_CODE (init) != TREE_VEC)
	    dump_generic_node (pp, init, spc, flags, false);
	  else
	    dump_omp_loop_non_rect_expr (pp, init, spc, flags);
	  pp_string (pp, "; ");

	  dump_generic_node (pp, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_space (pp);
	  switch (gimple_omp_for_cond (gs, i))
	    {
	    case LT_EXPR:
	      pp_less (pp);
	      break;
	    case GT_EXPR:
	      pp_greater (pp);
	      break;
	    case LE_EXPR:
	      pp_less_equal (pp);
	      break;
	    case GE_EXPR:
	      pp_greater_equal (pp);
	      break;
	    case NE_EXPR:
	      pp_string (pp, "!=");
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  pp_space (pp);
	  tree cond = gimple_omp_for_final (gs, i);
	  if (TREE_CODE (cond) != TREE_VEC)
	    dump_generic_node (pp, cond, spc, flags, false);
	  else
	    dump_omp_loop_non_rect_expr (pp, cond, spc, flags);
	  pp_string (pp, "; ");

	  dump_generic_node (pp, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_string (pp, " = ");
	  dump_generic_node (pp, gimple_omp_for_incr (gs, i), spc,
			     flags, false);
	  pp_right_paren (pp);
	}

      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_CONTINUE tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_continue (pretty_printer *pp, const gomp_continue *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%T, %T>", gs,
                       gimple_omp_continue_control_def (gs),
                       gimple_omp_continue_control_use (gs));
    }
  else
    {
      pp_string (pp, "#pragma omp continue (");
      dump_generic_node (pp, gimple_omp_continue_control_def (gs),
	  		 spc, flags, false);
      pp_comma (pp);
      pp_space (pp);
      dump_generic_node (pp, gimple_omp_continue_control_use (gs),
	  		 spc, flags, false);
      pp_right_paren (pp);
    }
}

/* Dump a GIMPLE_OMP_SINGLE tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_single (pretty_printer *pp, const gomp_single *gs,
			int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_single_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp single");
      dump_omp_clauses (pp, gimple_omp_single_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_TASKGROUP tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_taskgroup (pretty_printer *pp, const gimple *gs,
			   int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_taskgroup_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp taskgroup");
      dump_omp_clauses (pp, gimple_omp_taskgroup_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_MASKED tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_masked (pretty_printer *pp, const gimple *gs,
			int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_masked_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp masked");
      dump_omp_clauses (pp, gimple_omp_masked_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_SCOPE tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_scope (pretty_printer *pp, const gimple *gs,
		       int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_scope_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp scope");
      dump_omp_clauses (pp, gimple_omp_scope_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_DISPATCH tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_dispatch (pretty_printer *buffer, const gimple *gs, int spc,
			  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_dispatch_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >");
    }
  else
    {
      pp_string (buffer, "#pragma omp dispatch");
      dump_omp_clauses (buffer, gimple_omp_dispatch_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
    }
}

/* Dump a GIMPLE_OMP_TARGET tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_target (pretty_printer *pp, const gomp_target *gs,
			int spc, dump_flags_t flags)
{
  const char *kind;
  switch (gimple_omp_target_kind (gs))
    {
    case GF_OMP_TARGET_KIND_REGION:
      kind = "";
      break;
    case GF_OMP_TARGET_KIND_DATA:
      kind = " data";
      break;
    case GF_OMP_TARGET_KIND_UPDATE:
      kind = " update";
      break;
    case GF_OMP_TARGET_KIND_ENTER_DATA:
      kind = " enter data";
      break;
    case GF_OMP_TARGET_KIND_EXIT_DATA:
      kind = " exit data";
      break;
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
      kind = " oacc_kernels";
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
      kind = " oacc_parallel";
      break;
    case GF_OMP_TARGET_KIND_OACC_SERIAL:
      kind = " oacc_serial";
      break;
    case GF_OMP_TARGET_KIND_OACC_DATA:
      kind = " oacc_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
      kind = " oacc_update";
      break;
    case GF_OMP_TARGET_KIND_OACC_ENTER_DATA:
      kind = " oacc_enter_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_EXIT_DATA:
      kind = " oacc_exit_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      kind = " oacc_declare";
      break;
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      kind = " oacc_host_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL_KERNELS_PARALLELIZED:
      kind = " oacc_parallel_kernels_parallelized";
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL_KERNELS_GANG_SINGLE:
      kind = " oacc_parallel_kernels_gang_single";
      break;
    case GF_OMP_TARGET_KIND_OACC_DATA_KERNELS:
      kind = " oacc_data_kernels";
      break;
    default:
      gcc_unreachable ();
    }
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G%s <%+BODY <%S>%nCLAUSES <", gs,
		       kind, gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_target_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >, %T, %T%n>",
		       gimple_omp_target_child_fn (gs),
		       gimple_omp_target_data_arg (gs));
    }
  else
    {
      pp_string (pp, "#pragma omp target");
      pp_string (pp, kind);
      dump_omp_clauses (pp, gimple_omp_target_clauses (gs), spc, flags);
      if (gimple_omp_target_child_fn (gs))
	{
	  pp_string (pp, " [child fn: ");
	  dump_generic_node (pp, gimple_omp_target_child_fn (gs),
			     spc, flags, false);
	  pp_string (pp, " (");
	  if (gimple_omp_target_data_arg (gs))
	    dump_generic_node (pp, gimple_omp_target_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (pp, "???");
	  pp_string (pp, ")]");
	}
      gimple_seq body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
      else if (body)
	{
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 2, flags);
	}
    }
}

/* Dump a GIMPLE_OMP_TEAMS tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_teams (pretty_printer *pp, const gomp_teams *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_teams_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp teams");
      dump_omp_clauses (pp, gimple_omp_teams_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_character (pp, '{');
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_character (pp, '}');
	}
    }
}

/* Dump a GIMPLE_OMP_SECTIONS tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_sections (pretty_printer *pp, const gomp_sections *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_sections_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >");
    }
  else
    {
      pp_string (pp, "#pragma omp sections");
      if (gimple_omp_sections_control (gs))
	{
	  pp_string (pp, " <");
	  dump_generic_node (pp, gimple_omp_sections_control (gs), spc,
			     flags, false);
	  pp_greater (pp);
	}
      dump_omp_clauses (pp, gimple_omp_sections_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_{MASTER,ORDERED,SECTION,STRUCTURED_BLOCK} tuple on the
   pretty_printer PP.  */

static void
dump_gimple_omp_block (pretty_printer *pp, const gimple *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      switch (gimple_code (gs))
	{
	case GIMPLE_OMP_MASTER:
	  pp_string (pp, "#pragma omp master");
	  break;
	case GIMPLE_OMP_SECTION:
	  pp_string (pp, "#pragma omp section");
	  break;
	case GIMPLE_OMP_STRUCTURED_BLOCK:
	  pp_string (pp, "#pragma omp __structured_block");
	  break;
	default:
	  gcc_unreachable ();
	}
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_CRITICAL tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_critical (pretty_printer *pp, const gomp_critical *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      pp_string (pp, "#pragma omp critical");
      if (gimple_omp_critical_name (gs))
	{
	  pp_string (pp, " (");
	  dump_generic_node (pp, gimple_omp_critical_name (gs), spc,
			     flags, false);
	  pp_right_paren (pp);
	}
      dump_omp_clauses (pp, gimple_omp_critical_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_ORDERED tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_ordered (pretty_printer *pp, const gomp_ordered *gs,
			 int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      pp_string (pp, "#pragma omp ordered");
      dump_omp_clauses (pp, gimple_omp_ordered_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_SCAN tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_scan (pretty_printer *pp, const gomp_scan *gs,
		      int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      if (gimple_omp_scan_clauses (gs))
	{
	  pp_string (pp, "#pragma omp scan");
	  dump_omp_clauses (pp, gimple_omp_scan_clauses (gs), spc, flags);
	}
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
    }
}

/* Dump a GIMPLE_OMP_RETURN tuple on the pretty_printer PP.  */

static void
dump_gimple_omp_return (pretty_printer *pp, const gimple *gs, int spc,
			dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <nowait=%d", gs,
                       (int) gimple_omp_return_nowait_p (gs));
      if (gimple_omp_return_lhs (gs))
	dump_gimple_fmt (pp, spc, flags, ", lhs=%T>",
			 gimple_omp_return_lhs (gs));
      else
	dump_gimple_fmt (pp, spc, flags, ">");
    }
  else
    {
      pp_string (pp, "#pragma omp return");
      if (gimple_omp_return_nowait_p (gs))
	pp_string (pp, "(nowait)");
      if (gimple_omp_return_lhs (gs))
	{
	  pp_string (pp, " (set ");
	  dump_generic_node (pp, gimple_omp_return_lhs (gs),
			     spc, flags, false);
	  pp_character (pp, ')');
	}
    }
}

/* Dump a GIMPLE_ASSUME tuple on the pretty_printer PP.  */

static void
dump_gimple_assume (pretty_printer *pp, const gimple *gs,
		    int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags,
		     "%G [GUARD=%T] <%+BODY <%S> >",
		     gs, gimple_assume_guard (gs),
		     gimple_assume_body (gs));
  else
    {
      pp_string (pp, "[[assume (");
      dump_generic_node (pp, gimple_assume_guard (gs), spc, flags, false);
      pp_string (pp, ")]]");
      newline_and_indent (pp, spc + 2);
      pp_left_brace (pp);
      pp_newline (pp);
      dump_gimple_seq (pp, gimple_assume_body (gs), spc + 4, flags);
      newline_and_indent (pp, spc + 2);
      pp_right_brace (pp);
    }
}

/* Dump a GIMPLE_TRANSACTION tuple on the pretty_printer PP.  */

static void
dump_gimple_transaction (pretty_printer *pp, const gtransaction *gs,
			 int spc, dump_flags_t flags)
{
  unsigned subcode = gimple_transaction_subcode (gs);

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags,
		       "%G [SUBCODE=%x,NORM=%T,UNINST=%T,OVER=%T] "
		       "<%+BODY <%S> >",
		       gs, subcode, gimple_transaction_label_norm (gs),
		       gimple_transaction_label_uninst (gs),
		       gimple_transaction_label_over (gs),
		       gimple_transaction_body (gs));
    }
  else
    {
      if (subcode & GTMA_IS_OUTER)
	pp_string (pp, "__transaction_atomic [[outer]]");
      else if (subcode & GTMA_IS_RELAXED)
	pp_string (pp, "__transaction_relaxed");
      else
	pp_string (pp, "__transaction_atomic");
      subcode &= ~GTMA_DECLARATION_MASK;

      if (gimple_transaction_body (gs))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, gimple_transaction_body (gs),
			   spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
      else
	{
	  pp_string (pp, "  //");
	  if (gimple_transaction_label_norm (gs))
	    {
	      pp_string (pp, " NORM=");
	      dump_generic_node (pp, gimple_transaction_label_norm (gs),
				 spc, flags, false);
	    }
	  if (gimple_transaction_label_uninst (gs))
	    {
	      pp_string (pp, " UNINST=");
	      dump_generic_node (pp, gimple_transaction_label_uninst (gs),
				 spc, flags, false);
	    }
	  if (gimple_transaction_label_over (gs))
	    {
	      pp_string (pp, " OVER=");
	      dump_generic_node (pp, gimple_transaction_label_over (gs),
				 spc, flags, false);
	    }
	  if (subcode)
	    {
	      pp_string (pp, " SUBCODE=[ ");
	      if (subcode & GTMA_HAVE_ABORT)
		{
		  pp_string (pp, "GTMA_HAVE_ABORT ");
		  subcode &= ~GTMA_HAVE_ABORT;
		}
	      if (subcode & GTMA_HAVE_LOAD)
		{
		  pp_string (pp, "GTMA_HAVE_LOAD ");
		  subcode &= ~GTMA_HAVE_LOAD;
		}
	      if (subcode & GTMA_HAVE_STORE)
		{
		  pp_string (pp, "GTMA_HAVE_STORE ");
		  subcode &= ~GTMA_HAVE_STORE;
		}
	      if (subcode & GTMA_MAY_ENTER_IRREVOCABLE)
		{
		  pp_string (pp, "GTMA_MAY_ENTER_IRREVOCABLE ");
		  subcode &= ~GTMA_MAY_ENTER_IRREVOCABLE;
		}
	      if (subcode & GTMA_DOES_GO_IRREVOCABLE)
		{
		  pp_string (pp, "GTMA_DOES_GO_IRREVOCABLE ");
		  subcode &= ~GTMA_DOES_GO_IRREVOCABLE;
		}
	      if (subcode & GTMA_HAS_NO_INSTRUMENTATION)
		{
		  pp_string (pp, "GTMA_HAS_NO_INSTRUMENTATION ");
		  subcode &= ~GTMA_HAS_NO_INSTRUMENTATION;
		}
	      if (subcode)
		pp_printf (pp, "0x%x ", subcode);
	      pp_right_bracket (pp);
	    }
	}
    }
}

/* Dump a GIMPLE_ASM tuple on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_asm (pretty_printer *pp, const gasm *gs, int spc,
		 dump_flags_t flags)
{
  unsigned int i, n, f, fields;

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+STRING <%n%s%n>", gs,
                       gimple_asm_string (gs));

      n = gimple_asm_noutputs (gs);
      if (n)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_string (pp, "OUTPUT: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (pp, gimple_asm_output_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (pp, ", ");
	    }
	}

      n = gimple_asm_ninputs (gs);
      if (n)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_string (pp, "INPUT: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (pp, gimple_asm_input_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (pp, ", ");
	    }
	}

      n = gimple_asm_nclobbers (gs);
      if (n)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_string (pp, "CLOBBER: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (pp, gimple_asm_clobber_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (pp, ", ");
	    }
	}

      n = gimple_asm_nlabels (gs);
      if (n)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_string (pp, "LABEL: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (pp, gimple_asm_label_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (pp, ", ");
	    }
	}

      newline_and_indent (pp, spc);
      pp_greater (pp);
    }
  else
    {
      pp_string (pp, "__asm__");
      if (gimple_asm_volatile_p (gs))
	pp_string (pp, " __volatile__");
      if (gimple_asm_inline_p (gs))
	pp_string (pp, " __inline__");
      if (gimple_asm_nlabels (gs))
	pp_string (pp, " goto");
      pp_string (pp, "(\"");
      pp_string (pp, gimple_asm_string (gs));
      pp_string (pp, "\"");

      if (gimple_asm_nlabels (gs))
	fields = 4;
      else if (gimple_asm_nclobbers (gs))
	fields = 3;
      else if (gimple_asm_ninputs (gs))
	fields = 2;
      else if (gimple_asm_noutputs (gs))
	fields = 1;
      else
	fields = 0;

      for (f = 0; f < fields; ++f)
	{
	  pp_string (pp, " : ");

	  switch (f)
	    {
	    case 0:
	      n = gimple_asm_noutputs (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (pp, gimple_asm_output_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (pp, ", ");
		}
	      break;

	    case 1:
	      n = gimple_asm_ninputs (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (pp, gimple_asm_input_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (pp, ", ");
		}
	      break;

	    case 2:
	      n = gimple_asm_nclobbers (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (pp, gimple_asm_clobber_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (pp, ", ");
		}
	      break;

	    case 3:
	      n = gimple_asm_nlabels (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (pp, gimple_asm_label_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (pp, ", ");
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      pp_string (pp, ");");
    }
}

/* Dump ptr_info and range_info for NODE on pretty_printer PP with
   SPC spaces of indent.  */

static void
dump_ssaname_info (pretty_printer *pp, tree node, int spc)
{
  if (TREE_CODE (node) != SSA_NAME)
    return;

  if (POINTER_TYPE_P (TREE_TYPE (node))
      && SSA_NAME_PTR_INFO (node))
    {
      unsigned int align, misalign;
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (node);
      pp_string (pp, "# PT = ");
      pp_points_to_solution (pp, &pi->pt);
      newline_and_indent (pp, spc);
      if (get_ptr_info_alignment (pi, &align, &misalign))
	{
	  pp_printf (pp, "# ALIGN = %u, MISALIGN = %u", align, misalign);
	  newline_and_indent (pp, spc);
	}
    }

  if (!POINTER_TYPE_P (TREE_TYPE (node))
      && SSA_NAME_RANGE_INFO (node))
    {
      value_range r (TREE_TYPE (node));
      get_global_range_query ()->range_of_expr (r, node);
      pp_string (pp, "# RANGE ");
      pp_vrange (pp, &r);
      newline_and_indent (pp, spc);
    }
}

/* As dump_ssaname_info, but dump to FILE.  */

void
dump_ssaname_info_to_file (FILE *file, tree node, int spc)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  dump_ssaname_info (&pp, node, spc);
  pp_flush (&pp);
}

/* Dump a PHI node PHI.  PP, SPC and FLAGS are as in pp_gimple_stmt_1.
   The caller is responsible for calling pp_flush on PP to finalize
   pretty printer.  If COMMENT is true, print this after #.  */

static void
dump_gimple_phi (pretty_printer *pp, const gphi *phi, int spc, bool comment,
		 dump_flags_t flags)
{
  size_t i;
  tree lhs = gimple_phi_result (phi);

  if (flags & TDF_ALIAS)
    dump_ssaname_info (pp, lhs, spc);

  if (comment)
    pp_string (pp, "# ");

  if (flags & TDF_RAW)
    dump_gimple_fmt (pp, spc, flags, "%G <%T, ", phi,
		     gimple_phi_result (phi));
  else
    {
      dump_generic_node (pp, lhs, spc, flags, false);
      if (flags & TDF_GIMPLE)
	pp_string (pp, " = __PHI (");
      else
	pp_string (pp, " = PHI <");
    }
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      if ((flags & TDF_LINENO) && gimple_phi_arg_has_location (phi, i))
	dump_location (pp, gimple_phi_arg_location (phi, i));
      basic_block src = gimple_phi_arg_edge (phi, i)->src;
      if (flags & TDF_GIMPLE)
	{
	  pp_string (pp, "__BB");
	  pp_decimal_int (pp, src->index);
	  pp_string (pp, ": ");
	}
      dump_generic_node (pp, gimple_phi_arg_def (phi, i), spc, flags,
			 false);
      if (! (flags & TDF_GIMPLE))
	{
	  pp_left_paren (pp);
	  pp_decimal_int (pp, src->index);
	  pp_right_paren (pp);
	}
      if (i < gimple_phi_num_args (phi) - 1)
	pp_string (pp, ", ");
    }
  if (flags & TDF_GIMPLE)
    pp_string (pp, ");");
  else
    pp_greater (pp);
}


/* Dump a GIMPLE_OMP_PARALLEL tuple on the pretty_printer PP, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_omp_parallel (pretty_printer *pp, const gomp_parallel *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
                       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_parallel_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >, %T, %T%n>",
                       gimple_omp_parallel_child_fn (gs),
                       gimple_omp_parallel_data_arg (gs));
    }
  else
    {
      gimple_seq body;
      pp_string (pp, "#pragma omp parallel");
      dump_omp_clauses (pp, gimple_omp_parallel_clauses (gs), spc, flags);
      if (gimple_omp_parallel_child_fn (gs))
	{
	  pp_string (pp, " [child fn: ");
	  dump_generic_node (pp, gimple_omp_parallel_child_fn (gs),
			     spc, flags, false);
	  pp_string (pp, " (");
	  if (gimple_omp_parallel_data_arg (gs))
	    dump_generic_node (pp, gimple_omp_parallel_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (pp, "???");
	  pp_string (pp, ")]");
	}
      body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
      else if (body)
	{
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 2, flags);
	}
    }
}


/* Dump a GIMPLE_OMP_TASK tuple on the pretty_printer PP, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_omp_task (pretty_printer *pp, const gomp_task *gs, int spc,
		      dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
                       gimple_omp_body (gs));
      dump_omp_clauses (pp, gimple_omp_task_clauses (gs), spc, flags);
      dump_gimple_fmt (pp, spc, flags, " >, %T, %T, %T, %T, %T%n>",
                       gimple_omp_task_child_fn (gs),
                       gimple_omp_task_data_arg (gs),
		       gimple_omp_task_copy_fn (gs),
		       gimple_omp_task_arg_size (gs),
		       gimple_omp_task_arg_size (gs));
    }
  else
    {
      gimple_seq body;
      if (gimple_omp_task_taskloop_p (gs))
	pp_string (pp, "#pragma omp taskloop");
      else if (gimple_omp_task_taskwait_p (gs))
	pp_string (pp, "#pragma omp taskwait");
      else
	pp_string (pp, "#pragma omp task");
      dump_omp_clauses (pp, gimple_omp_task_clauses (gs), spc, flags);
      if (gimple_omp_task_child_fn (gs))
	{
	  pp_string (pp, " [child fn: ");
	  dump_generic_node (pp, gimple_omp_task_child_fn (gs),
			     spc, flags, false);
	  pp_string (pp, " (");
	  if (gimple_omp_task_data_arg (gs))
	    dump_generic_node (pp, gimple_omp_task_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (pp, "???");
	  pp_string (pp, ")]");
	}
      body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 4, flags);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
      else if (body)
	{
	  pp_newline (pp);
	  dump_gimple_seq (pp, body, spc + 2, flags);
	}
    }
}


/* Dump a GIMPLE_OMP_ATOMIC_LOAD tuple on the pretty_printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_omp_atomic_load (pretty_printer *pp, const gomp_atomic_load *gs,
			     int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%T, %T>", gs,
                       gimple_omp_atomic_load_lhs (gs),
                       gimple_omp_atomic_load_rhs (gs));
    }
  else
    {
      pp_string (pp, "#pragma omp atomic_load");
      dump_omp_atomic_memory_order (pp,
				    gimple_omp_atomic_memory_order (gs));
      if (gimple_omp_atomic_need_value_p (gs))
	pp_string (pp, " [needed]");
      if (gimple_omp_atomic_weak_p (gs))
	pp_string (pp, " [weak]");
      newline_and_indent (pp, spc + 2);
      dump_generic_node (pp, gimple_omp_atomic_load_lhs (gs),
	  		 spc, flags, false);
      pp_space (pp);
      pp_equal (pp);
      pp_space (pp);
      pp_star (pp);
      dump_generic_node (pp, gimple_omp_atomic_load_rhs (gs),
	  		 spc, flags, false);
    }
}

/* Dump a GIMPLE_OMP_ATOMIC_STORE tuple on the pretty_printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_omp_atomic_store (pretty_printer *pp,
			      const gomp_atomic_store *gs, int spc,
			      dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (pp, spc, flags, "%G <%T>", gs,
                       gimple_omp_atomic_store_val (gs));
    }
  else
    {
      pp_string (pp, "#pragma omp atomic_store");
      dump_omp_atomic_memory_order (pp,
				    gimple_omp_atomic_memory_order (gs));
      pp_space (pp);
      if (gimple_omp_atomic_need_value_p (gs))
	pp_string (pp, "[needed] ");
      if (gimple_omp_atomic_weak_p (gs))
	pp_string (pp, "[weak] ");
      pp_left_paren (pp);
      dump_generic_node (pp, gimple_omp_atomic_store_val (gs),
	  		 spc, flags, false);
      pp_right_paren (pp);
    }
}


/* Dump all the memory operands for statement GS.  PP, SPC and
   FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_gimple_mem_ops (pretty_printer *pp, const gimple *gs, int spc,
		     dump_flags_t flags)
{
  tree vdef = gimple_vdef (gs);
  tree vuse = gimple_vuse (gs);

  if (vdef != NULL_TREE)
    {
      pp_string (pp, "# ");
      dump_generic_node (pp, vdef, spc + 2, flags, false);
      pp_string (pp, " = VDEF <");
      dump_generic_node (pp, vuse, spc + 2, flags, false);
      pp_greater (pp);
      newline_and_indent (pp, spc);
    }
  else if (vuse != NULL_TREE)
    {
      pp_string (pp, "# VUSE <");
      dump_generic_node (pp, vuse, spc + 2, flags, false);
      pp_greater (pp);
      newline_and_indent (pp, spc);
    }
}


/* Print the gimple statement GS on the pretty printer PP, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  The caller is responsible for calling
   pp_flush on PP to finalize the pretty printer.  */

void
pp_gimple_stmt_1 (pretty_printer *pp, const gimple *gs, int spc,
		  dump_flags_t flags)
{
  if (!gs)
    return;

  if (flags & TDF_STMTADDR)
    pp_printf (pp, "<&%p> ", (const void *) gs);

  if ((flags & TDF_LINENO) && gimple_has_location (gs))
    dump_location (pp, gimple_location (gs));

  if (flags & TDF_EH)
    {
      int lp_nr = lookup_stmt_eh_lp (gs);
      if (lp_nr > 0)
	pp_printf (pp, "[LP %d] ", lp_nr);
      else if (lp_nr < 0)
	pp_printf (pp, "[MNT %d] ", -lp_nr);
    }

  if ((flags & (TDF_VOPS|TDF_MEMSYMS))
      && gimple_has_mem_ops (gs))
    dump_gimple_mem_ops (pp, gs, spc, flags);

  if (gimple_has_lhs (gs)
      && (flags & TDF_ALIAS))
    dump_ssaname_info (pp, gimple_get_lhs (gs), spc);

  switch (gimple_code (gs))
    {
    case GIMPLE_ASM:
      dump_gimple_asm (pp, as_a <const gasm *> (gs), spc, flags);
      break;

    case GIMPLE_ASSIGN:
      dump_gimple_assign (pp, as_a <const gassign *> (gs), spc, flags);
      break;

    case GIMPLE_BIND:
      dump_gimple_bind (pp, as_a <const gbind *> (gs), spc, flags);
      break;

    case GIMPLE_CALL:
      dump_gimple_call (pp, as_a <const gcall *> (gs), spc, flags);
      break;

    case GIMPLE_COND:
      dump_gimple_cond (pp, as_a <const gcond *> (gs), spc, flags);
      break;

    case GIMPLE_LABEL:
      dump_gimple_label (pp, as_a <const glabel *> (gs), spc, flags);
      break;

    case GIMPLE_GOTO:
      dump_gimple_goto (pp, as_a <const ggoto *> (gs), spc, flags);
      break;

    case GIMPLE_NOP:
      pp_string (pp, "GIMPLE_NOP");
      break;

    case GIMPLE_RETURN:
      dump_gimple_return (pp, as_a <const greturn *> (gs), spc, flags);
      break;

    case GIMPLE_SWITCH:
      dump_gimple_switch (pp, as_a <const gswitch *> (gs), spc, flags);
      break;

    case GIMPLE_TRY:
      dump_gimple_try (pp, as_a <const gtry *> (gs), spc, flags);
      break;

    case GIMPLE_PHI:
      dump_gimple_phi (pp, as_a <const gphi *> (gs), spc, false, flags);
      break;

    case GIMPLE_OMP_PARALLEL:
      dump_gimple_omp_parallel (pp, as_a <const gomp_parallel *> (gs), spc,
				flags);
      break;

    case GIMPLE_OMP_TASK:
      dump_gimple_omp_task (pp, as_a <const gomp_task *> (gs), spc, flags);
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
      dump_gimple_omp_atomic_load (pp, as_a <const gomp_atomic_load *> (gs),
				   spc, flags);
      break;

    case GIMPLE_OMP_ATOMIC_STORE:
      dump_gimple_omp_atomic_store (pp,
				    as_a <const gomp_atomic_store *> (gs),
				    spc, flags);
      break;

    case GIMPLE_OMP_FOR:
      dump_gimple_omp_for (pp, as_a <const gomp_for *> (gs), spc, flags);
      break;

    case GIMPLE_OMP_CONTINUE:
      dump_gimple_omp_continue (pp, as_a <const gomp_continue *> (gs), spc,
				flags);
      break;

    case GIMPLE_OMP_SINGLE:
      dump_gimple_omp_single (pp, as_a <const gomp_single *> (gs), spc,
			      flags);
      break;

    case GIMPLE_OMP_TARGET:
      dump_gimple_omp_target (pp, as_a <const gomp_target *> (gs), spc,
			      flags);
      break;

    case GIMPLE_OMP_TEAMS:
      dump_gimple_omp_teams (pp, as_a <const gomp_teams *> (gs), spc,
			     flags);
      break;

    case GIMPLE_OMP_RETURN:
      dump_gimple_omp_return (pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS:
      dump_gimple_omp_sections (pp, as_a <const gomp_sections *> (gs),
				spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS_SWITCH:
      pp_string (pp, "GIMPLE_SECTIONS_SWITCH");
      break;

    case GIMPLE_OMP_TASKGROUP:
      dump_gimple_omp_taskgroup (pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_MASKED:
      dump_gimple_omp_masked (pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_SCOPE:
      dump_gimple_omp_scope (pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_DISPATCH:
      dump_gimple_omp_dispatch(pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_STRUCTURED_BLOCK:
      dump_gimple_omp_block (pp, gs, spc, flags);
      break;

    case GIMPLE_OMP_ORDERED:
      dump_gimple_omp_ordered (pp, as_a <const gomp_ordered *> (gs), spc,
			       flags);
      break;

    case GIMPLE_OMP_SCAN:
      dump_gimple_omp_scan (pp, as_a <const gomp_scan *> (gs), spc,
			    flags);
      break;

    case GIMPLE_OMP_CRITICAL:
      dump_gimple_omp_critical (pp, as_a <const gomp_critical *> (gs), spc,
				flags);
      break;

    case GIMPLE_CATCH:
      dump_gimple_catch (pp, as_a <const gcatch *> (gs), spc, flags);
      break;

    case GIMPLE_EH_FILTER:
      dump_gimple_eh_filter (pp, as_a <const geh_filter *> (gs), spc,
			     flags);
      break;

    case GIMPLE_EH_MUST_NOT_THROW:
      dump_gimple_eh_must_not_throw (pp,
				     as_a <const geh_mnt *> (gs),
				     spc, flags);
      break;

    case GIMPLE_EH_ELSE:
      dump_gimple_eh_else (pp, as_a <const geh_else *> (gs), spc, flags);
      break;

    case GIMPLE_RESX:
      dump_gimple_resx (pp, as_a <const gresx *> (gs), spc, flags);
      break;

    case GIMPLE_EH_DISPATCH:
      dump_gimple_eh_dispatch (pp, as_a <const geh_dispatch *> (gs), spc,
			       flags);
      break;

    case GIMPLE_DEBUG:
      dump_gimple_debug (pp, as_a <const gdebug *> (gs), spc, flags);
      break;

    case GIMPLE_PREDICT:
      pp_string (pp, "// predicted ");
      if (gimple_predict_outcome (gs))
	pp_string (pp, "likely by ");
      else
	pp_string (pp, "unlikely by ");
      pp_string (pp, predictor_name (gimple_predict_predictor (gs)));
      pp_string (pp, " predictor.");
      break;

    case GIMPLE_ASSUME:
      dump_gimple_assume (pp, gs, spc, flags);
      break;

    case GIMPLE_TRANSACTION:
      dump_gimple_transaction (pp, as_a <const gtransaction *> (gs), spc,
			       flags);
      break;

    default:
      GIMPLE_NIY;
    }
}


/* Dumps header of basic block BB to OUTF indented by INDENT
   spaces and details described by flags.  */

static void
dump_gimple_bb_header (FILE *outf, basic_block bb, int indent,
		       dump_flags_t flags)
{
  if (flags & TDF_BLOCKS)
    {
      if (flags & TDF_LINENO)
	{
	  gimple_stmt_iterator gsi;

	  fputs (";; ", outf);

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    if (!is_gimple_debug (gsi_stmt (gsi))
		&& get_lineno (gsi_stmt (gsi)) != UNKNOWN_LOCATION)
	      {
		fprintf (outf, "%*sstarting at line %d",
			 indent, "", get_lineno (gsi_stmt (gsi)));
		break;
	      }
	  fputc ('\n', outf);
	}
    }
  else
    {
      if (flags & TDF_GIMPLE)
	{
	  fprintf (outf, "%*s__BB(%d", indent, "", bb->index);
	  if (bb->loop_father->header == bb)
	    fprintf (outf, ",loop_header(%d)", bb->loop_father->num);
	  if (bb->count.initialized_p ())
	    fprintf (outf, ",%s(%" PRIu64 ")",
		     profile_quality_as_string (bb->count.quality ()),
		     bb->count.value ());
	  fprintf (outf, "):\n");
	}
      else
	fprintf (outf, "%*s<bb %d> %s:\n",
		 indent, "", bb->index, dump_profile (bb->count));
    }
}


/* Dumps end of basic block BB to PP indented by INDENT
   spaces.  */

static void
dump_gimple_bb_footer (FILE *outf ATTRIBUTE_UNUSED,
		       basic_block bb ATTRIBUTE_UNUSED,
		       int indent ATTRIBUTE_UNUSED,
		       dump_flags_t flags ATTRIBUTE_UNUSED)
{
  /* There is currently no GIMPLE-specific basic block info to dump.  */
  return;
}


/* Dump PHI nodes of basic block BB to PP with details described
   by FLAGS and indented by INDENT spaces.  */

static void
dump_phi_nodes (pretty_printer *pp, basic_block bb, int indent,
		dump_flags_t flags)
{
  gphi_iterator i;

  for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
    {
      gphi *phi = i.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)) || (flags & TDF_VOPS))
        {
          INDENT (indent);
	  dump_gimple_phi (pp, phi, indent,
			   (flags & TDF_GIMPLE) ? false : true, flags);
          pp_newline (pp);
        }
    }
}


/* Dump jump to basic block BB that is represented implicitly in the cfg
   to PP.  */

static void
pp_cfg_jump (pretty_printer *pp, edge e, dump_flags_t flags)
{
  if (flags & TDF_GIMPLE)
    {
      pp_string (pp, "goto __BB");
      pp_decimal_int (pp, e->dest->index);
      if (e->probability.initialized_p ())
	{
	  pp_string (pp, "(");
	  pp_string (pp,
		     profile_quality_as_string (e->probability.quality ()));
	  pp_string (pp, "(");
	  pp_decimal_int (pp, e->probability.value ());
	  pp_string (pp, "))");
	}
      pp_semicolon (pp);
    }
  else
    {
      pp_string (pp, "goto <bb ");
      pp_decimal_int (pp, e->dest->index);
      pp_greater (pp);
      pp_semicolon (pp);

      dump_edge_probability (pp, e);
    }
}


/* Dump edges represented implicitly in basic block BB to PP, indented
   by INDENT spaces, with details given by FLAGS.  */

static void
dump_implicit_edges (pretty_printer *pp, basic_block bb, int indent,
		     dump_flags_t flags)
{
  edge e;

  if (safe_is_a <gcond *> (*gsi_last_bb (bb)))
    {
      edge true_edge, false_edge;

      /* When we are emitting the code or changing CFG, it is possible that
	 the edges are not yet created.  When we are using debug_bb in such
	 a situation, we do not want it to crash.  */
      if (EDGE_COUNT (bb->succs) != 2)
	return;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      INDENT (indent + 2);
      pp_cfg_jump (pp, true_edge, flags);
      newline_and_indent (pp, indent);
      pp_string (pp, "else");
      newline_and_indent (pp, indent + 2);
      pp_cfg_jump (pp, false_edge, flags);
      pp_newline (pp);
      return;
    }

  /* If there is a fallthru edge, we may need to add an artificial
     goto to the dump.  */
  e = find_fallthru_edge (bb->succs);

  if (e && (e->dest != bb->next_bb || (flags & TDF_GIMPLE)))
    {
      INDENT (indent);

      if ((flags & TDF_LINENO)
	  && e->goto_locus != UNKNOWN_LOCATION)
	dump_location (pp, e->goto_locus);

      pp_cfg_jump (pp, e, flags);
      pp_newline (pp);
    }
}


/* Dumps basic block BB to PP with details described by FLAGS and
   indented by INDENT spaces.  */

static void
gimple_dump_bb_buff (pretty_printer *pp, basic_block bb, int indent,
		     dump_flags_t flags)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  int label_indent = indent - 2;

  if (label_indent < 0)
    label_indent = 0;

  dump_phi_nodes (pp, bb, indent, flags);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      int curr_indent;

      stmt = gsi_stmt (gsi);

      curr_indent = gimple_code (stmt) == GIMPLE_LABEL ? label_indent : indent;

      INDENT (curr_indent);
      pp_gimple_stmt_1 (pp, stmt, curr_indent, flags);
      pp_newline_and_flush (pp);
      gcc_checking_assert (DECL_STRUCT_FUNCTION (current_function_decl));
      dump_histograms_for_stmt (DECL_STRUCT_FUNCTION (current_function_decl),
				pp_buffer (pp)->m_stream, stmt);
    }

  dump_implicit_edges (pp, bb, indent, flags);
  pp_flush (pp);
}


/* Dumps basic block BB to FILE with details described by FLAGS and
   indented by INDENT spaces.  */

void
gimple_dump_bb (FILE *file, basic_block bb, int indent, dump_flags_t flags)
{
  dump_gimple_bb_header (file, bb, indent, flags);
  if (bb->index >= NUM_FIXED_BLOCKS)
    {
      pretty_printer pp;
      pp_needs_newline (&pp) = true;
      pp.set_output_stream (file);
      gimple_dump_bb_buff (&pp, bb, indent, flags);
    }
  dump_gimple_bb_footer (file, bb, indent, flags);
}

/* Dumps basic block BB to pretty-printer PP with default dump flags and
   no indentation, for use as a label of a DOT graph record-node.
   ??? Should just use gimple_dump_bb_buff here, except that value profiling
   histogram dumping doesn't know about pretty-printers.  */

void
gimple_dump_bb_for_graph (pretty_printer *pp, basic_block bb)
{
  pp_printf (pp, "<bb %d>:\n", bb->index);
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);

  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi))
	  || (dump_flags & TDF_VOPS))
	{
	  pp_bar (pp);
	  pp_write_text_to_stream (pp);
	  pp_string (pp, "# ");
	  pp_gimple_stmt_1 (pp, phi, 0, dump_flags);
	  pp_newline (pp);
	  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);
	}
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      pp_bar (pp);
      pp_write_text_to_stream (pp);
      pp_gimple_stmt_1 (pp, stmt, 0, dump_flags);
      pp_newline (pp);
      pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);
    }
  dump_implicit_edges (pp, bb, 0, dump_flags);
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
