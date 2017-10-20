/* Pretty formatting of GIMPLE statements and expressions.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

#define INDENT(SPACE)							\
  do { int i; for (i = 0; i < SPACE; i++) pp_space (buffer); } while (0)

#define GIMPLE_NIY do_niy (buffer,gs)

/* Try to print on BUFFER a default message for the unrecognized
   gimple statement GS.  */

static void
do_niy (pretty_printer *buffer, gimple *gs)
{
  pp_printf (buffer, "<<< Unknown GIMPLE statement: %s >>>\n",
	     gimple_code_name[(int) gimple_code (gs)]);
}


/* Emit a newline and SPC indentation spaces to BUFFER.  */

static void
newline_and_indent (pretty_printer *buffer, int spc)
{
  pp_newline (buffer);
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
dump_profile (int frequency, profile_count &count)
{
  float minimum = 0.01f;

  gcc_assert (0 <= frequency && frequency <= REG_BR_PROB_BASE);
  float fvalue = frequency * 100.0f / REG_BR_PROB_BASE;
  if (fvalue < minimum && frequency > 0)
    return "[0.01%]";

  char *buf;
  if (count.initialized_p ())
    buf = xasprintf ("[%.2f%%] [count: %" PRId64 "]", fvalue,
		     count.to_gcov_type ());
  else
    buf = xasprintf ("[%.2f%%] [count: INV]", fvalue);

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

/* Dump E probability to BUFFER.  */

static void
dump_edge_probability (pretty_printer *buffer, edge e)
{
  pp_scalar (buffer, " %s", dump_probability (e->probability));
}

/* Print GIMPLE statement G to FILE using SPC indentation spaces and
   FLAGS as in pp_gimple_stmt_1.  */

void
print_gimple_stmt (FILE *file, gimple *g, int spc, dump_flags_t flags)
{
  pretty_printer buffer;
  pp_needs_newline (&buffer) = true;
  buffer.buffer->stream = file;
  pp_gimple_stmt_1 (&buffer, g, spc, flags);
  pp_newline_and_flush (&buffer);
}

DEBUG_FUNCTION void
debug (gimple &ref)
{
  print_gimple_stmt (stderr, &ref, 0, 0);
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
  pretty_printer buffer;
  pp_needs_newline (&buffer) = true;
  buffer.buffer->stream = file;
  pp_gimple_stmt_1 (&buffer, g, spc, flags);
  pp_flush (&buffer);
}


/* Print the GIMPLE sequence SEQ on BUFFER using SPC indentation
   spaces and FLAGS as in pp_gimple_stmt_1.
   The caller is responsible for calling pp_flush on BUFFER to finalize
   the pretty printer.  */

static void
dump_gimple_seq (pretty_printer *buffer, gimple_seq seq, int spc,
		 dump_flags_t flags)
{
  gimple_stmt_iterator i;

  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gimple *gs = gsi_stmt (i);
      INDENT (spc);
      pp_gimple_stmt_1 (buffer, gs, spc, flags);
      if (!gsi_one_before_end_p (i))
	pp_newline (buffer);
    }
}


/* Print GIMPLE sequence SEQ to FILE using SPC indentation spaces and
   FLAGS as in pp_gimple_stmt_1.  */

void
print_gimple_seq (FILE *file, gimple_seq seq, int spc, dump_flags_t flags)
{
  pretty_printer buffer;
  pp_needs_newline (&buffer) = true;
  buffer.buffer->stream = file;
  dump_gimple_seq (&buffer, seq, spc, flags);
  pp_newline_and_flush (&buffer);
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
dump_gimple_fmt (pretty_printer *buffer, int spc, dump_flags_t flags,
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
                pp_string (buffer, tmp);
                break;

              case 'S':
                seq = va_arg (args, gimple_seq);
                pp_newline (buffer);
                dump_gimple_seq (buffer, seq, spc + 2, flags);
                newline_and_indent (buffer, spc);
                break;

              case 'T':
                t = va_arg (args, tree);
                if (t == NULL_TREE)
                  pp_string (buffer, "NULL");
                else
                  dump_generic_node (buffer, t, spc, flags, false);
                break;

              case 'd':
                pp_decimal_int (buffer, va_arg (args, int));
                break;

              case 's':
                pp_string (buffer, va_arg (args, char *));
                break;

              case 'n':
                newline_and_indent (buffer, spc);
                break;

	      case 'x':
		pp_scalar (buffer, "%x", va_arg (args, int));
		break;

              case '+':
                spc += 2;
                newline_and_indent (buffer, spc);
                break;

              case '-':
                spc -= 2;
                newline_and_indent (buffer, spc);
                break;

              default:
                gcc_unreachable ();
            }
        }
      else
        pp_character (buffer, *c);
    }
  va_end (args);
}


/* Helper for dump_gimple_assign.  Print the unary RHS of the
   assignment GS.  BUFFER, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_unary_rhs (pretty_printer *buffer, gassign *gs, int spc,
		dump_flags_t flags)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (gs);
  tree lhs = gimple_assign_lhs (gs);
  tree rhs = gimple_assign_rhs1 (gs);

  switch (rhs_code)
    {
    case VIEW_CONVERT_EXPR:
    case ASSERT_EXPR:
      dump_generic_node (buffer, rhs, spc, flags, false);
      break;

    case FIXED_CONVERT_EXPR:
    case ADDR_SPACE_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
      pp_left_paren (buffer);
      dump_generic_node (buffer, TREE_TYPE (lhs), spc, flags, false);
      pp_string (buffer, ") ");
      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_left_paren (buffer);
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  pp_right_paren (buffer);
	}
      else
	dump_generic_node (buffer, rhs, spc, flags, false);
      break;

    case PAREN_EXPR:
      pp_string (buffer, "((");
      dump_generic_node (buffer, rhs, spc, flags, false);
      pp_string (buffer, "))");
      break;

    case ABS_EXPR:
      if (flags & TDF_GIMPLE)
	{
	  pp_string (buffer, "__ABS ");
	  dump_generic_node (buffer, rhs, spc, flags, false);
	}
      else
	{
	  pp_string (buffer, "ABS_EXPR <");
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  pp_greater (buffer);
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
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  break;
	}
      else if (rhs_code == BIT_NOT_EXPR)
	pp_complement (buffer);
      else if (rhs_code == TRUTH_NOT_EXPR)
	pp_exclamation (buffer);
      else if (rhs_code == NEGATE_EXPR)
	pp_minus (buffer);
      else
	{
	  pp_left_bracket (buffer);
	  pp_string (buffer, get_tree_code_name (rhs_code));
	  pp_string (buffer, "] ");
	}

      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_left_paren (buffer);
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  pp_right_paren (buffer);
	}
      else
	dump_generic_node (buffer, rhs, spc, flags, false);
      break;
    }
}


/* Helper for dump_gimple_assign.  Print the binary RHS of the
   assignment GS.  BUFFER, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_binary_rhs (pretty_printer *buffer, gassign *gs, int spc,
		 dump_flags_t flags)
{
  const char *p;
  enum tree_code code = gimple_assign_rhs_code (gs);
  switch (code)
    {
    case COMPLEX_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
      for (p = get_tree_code_name (code); *p; p++)
	pp_character (buffer, TOUPPER (*p));
      pp_string (buffer, " <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    default:
      if (op_prio (gimple_assign_rhs1 (gs)) <= op_code_prio (code))
	{
	  pp_left_paren (buffer);
	  dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags,
			     false);
	  pp_right_paren (buffer);
	}
      else
	dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_space (buffer);
      pp_string (buffer, op_symbol_code (gimple_assign_rhs_code (gs)));
      pp_space (buffer);
      if (op_prio (gimple_assign_rhs2 (gs)) <= op_code_prio (code))
	{
	  pp_left_paren (buffer);
	  dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags,
			     false);
	  pp_right_paren (buffer);
	}
      else
	dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
    }
}

/* Helper for dump_gimple_assign.  Print the ternary RHS of the
   assignment GS.  BUFFER, SPC and FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_ternary_rhs (pretty_printer *buffer, gassign *gs, int spc,
		  dump_flags_t flags)
{
  const char *p;
  enum tree_code code = gimple_assign_rhs_code (gs);
  switch (code)
    {
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
      for (p = get_tree_code_name (code); *p; p++)
	pp_character (buffer, TOUPPER (*p));
      pp_string (buffer, " <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    case FMA_EXPR:
      if (flags & TDF_GIMPLE)
	{
	  pp_string (buffer, "__FMA (");
	  dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
	  pp_comma (buffer);
	  dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
	  pp_comma (buffer);
	  dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
	  pp_right_paren (buffer);
	}
      else
	{
	  dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
	  pp_string (buffer, " * ");
	  dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
	  pp_string (buffer, " + ");
	  dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
	}
      break;

    case DOT_PROD_EXPR:
      pp_string (buffer, "DOT_PROD_EXPR <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    case SAD_EXPR:
      pp_string (buffer, "SAD_EXPR <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;
    
    case VEC_PERM_EXPR:
      pp_string (buffer, "VEC_PERM_EXPR <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    case REALIGN_LOAD_EXPR:
      pp_string (buffer, "REALIGN_LOAD <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    case COND_EXPR:
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, " ? ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, " : ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      break;

    case VEC_COND_EXPR:
      pp_string (buffer, "VEC_COND_EXPR <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_greater (buffer);
      break;

    case BIT_INSERT_EXPR:
      pp_string (buffer, "BIT_INSERT_EXPR <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs3 (gs), spc, flags, false);
      pp_string (buffer, " (");
      if (INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs2 (gs))))
	pp_decimal_int (buffer,
			TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs2 (gs))));
      else
	dump_generic_node (buffer,
			   TYPE_SIZE (TREE_TYPE (gimple_assign_rhs2 (gs))),
			   spc, flags, false);
      pp_string (buffer, " bits)>");
      break;

    default:
      gcc_unreachable ();
    }
}


/* Dump the gimple assignment GS.  BUFFER, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_assign (pretty_printer *buffer, gassign *gs, int spc,
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

      dump_gimple_fmt (buffer, spc, flags, "%G <%s, %T, %T, %T, %T>", gs,
		       get_tree_code_name (gimple_assign_rhs_code (gs)),
                       gimple_assign_lhs (gs), arg1, arg2, arg3);
    }
  else
    {
      if (!(flags & TDF_RHS_ONLY))
	{
	  dump_generic_node (buffer, gimple_assign_lhs (gs), spc, flags, false);
	  pp_space (buffer);
	  pp_equal (buffer);

	  if (gimple_assign_nontemporal_move_p (gs))
	    pp_string (buffer, "{nt}");

	  if (gimple_has_volatile_ops (gs))
	    pp_string (buffer, "{v}");

	  pp_space (buffer);
	}

      if (gimple_num_ops (gs) == 2)
        dump_unary_rhs (buffer, gs, spc, flags);
      else if (gimple_num_ops (gs) == 3)
        dump_binary_rhs (buffer, gs, spc, flags);
      else if (gimple_num_ops (gs) == 4)
        dump_ternary_rhs (buffer, gs, spc, flags);
      else
        gcc_unreachable ();
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon (buffer);
    }
}


/* Dump the return statement GS.  BUFFER, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_return (pretty_printer *buffer, greturn *gs, int spc,
		    dump_flags_t flags)
{
  tree t, t2;

  t = gimple_return_retval (gs);
  t2 = gimple_return_retbnd (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T %T>", gs, t, t2);
  else
    {
      pp_string (buffer, "return");
      if (t)
	{
	  pp_space (buffer);
	  dump_generic_node (buffer, t, spc, flags, false);
	}
      if (t2)
	{
	  pp_string (buffer, ", ");
	  dump_generic_node (buffer, t2, spc, flags, false);
	}
      pp_semicolon (buffer);
    }
}


/* Dump the call arguments for a gimple call. BUFFER, FLAGS are as in
   dump_gimple_call.  */

static void
dump_gimple_call_args (pretty_printer *buffer, gcall *gs, dump_flags_t flags)
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
	      pp_string (buffer, enums[v]);
	    }
	}
    }

  for (; i < gimple_call_num_args (gs); i++)
    {
      if (i)
	pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_call_arg (gs, i), 0, flags, false);
    }

  if (gimple_call_va_arg_pack_p (gs))
    {
      if (i)
	pp_string (buffer, ", ");

      pp_string (buffer, "__builtin_va_arg_pack ()");
    }
}

/* Dump the points-to solution *PT to BUFFER.  */

static void
pp_points_to_solution (pretty_printer *buffer, struct pt_solution *pt)
{
  if (pt->anything)
    {
      pp_string (buffer, "anything ");
      return;
    }
  if (pt->nonlocal)
    pp_string (buffer, "nonlocal ");
  if (pt->escaped)
    pp_string (buffer, "escaped ");
  if (pt->ipa_escaped)
    pp_string (buffer, "unit-escaped ");
  if (pt->null)
    pp_string (buffer, "null ");
  if (pt->vars
      && !bitmap_empty_p (pt->vars))
    {
      bitmap_iterator bi;
      unsigned i;
      pp_string (buffer, "{ ");
      EXECUTE_IF_SET_IN_BITMAP (pt->vars, 0, i, bi)
	{
	  pp_string (buffer, "D.");
	  pp_decimal_int (buffer, i);
	  pp_space (buffer);
	}
      pp_right_brace (buffer);
      if (pt->vars_contains_nonlocal
	  || pt->vars_contains_escaped
	  || pt->vars_contains_escaped_heap
	  || pt->vars_contains_restrict)
	{
	  const char *comma = "";
	  pp_string (buffer, " (");
	  if (pt->vars_contains_nonlocal)
	    {
	      pp_string (buffer, "nonlocal");
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped)
	    {
	      pp_string (buffer, comma);
	      pp_string (buffer, "escaped");
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped_heap)
	    {
	      pp_string (buffer, comma);
	      pp_string (buffer, "escaped heap");
	      comma = ", ";
	    }
	  if (pt->vars_contains_restrict)
	    {
	      pp_string (buffer, comma);
	      pp_string (buffer, "restrict");
	      comma = ", ";
	    }
	  if (pt->vars_contains_interposable)
	    {
	      pp_string (buffer, comma);
	      pp_string (buffer, "interposable");
	    }
	  pp_string (buffer, ")");
	}

    }
}

/* Dump the call statement GS.  BUFFER, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_call (pretty_printer *buffer, gcall *gs, int spc,
		  dump_flags_t flags)
{
  tree lhs = gimple_call_lhs (gs);
  tree fn = gimple_call_fn (gs);

  if (flags & TDF_ALIAS)
    {
      struct pt_solution *pt;
      pt = gimple_call_use_set (gs);
      if (!pt_solution_empty_p (pt))
	{
	  pp_string (buffer, "# USE = ");
	  pp_points_to_solution (buffer, pt);
	  newline_and_indent (buffer, spc);
	}
      pt = gimple_call_clobber_set (gs);
      if (!pt_solution_empty_p (pt))
	{
	  pp_string (buffer, "# CLB = ");
	  pp_points_to_solution (buffer, pt);
	  newline_and_indent (buffer, spc);
	}
    }

  if (flags & TDF_RAW)
    {
      if (gimple_call_internal_p (gs))
	dump_gimple_fmt (buffer, spc, flags, "%G <%s, %T", gs,
			 internal_fn_name (gimple_call_internal_fn (gs)), lhs);
      else
	dump_gimple_fmt (buffer, spc, flags, "%G <%T, %T", gs, fn, lhs);
      if (gimple_call_num_args (gs) > 0)
        {
          pp_string (buffer, ", ");
          dump_gimple_call_args (buffer, gs, flags);
        }
      pp_greater (buffer);
    }
  else
    {
      if (lhs && !(flags & TDF_RHS_ONLY))
        {
          dump_generic_node (buffer, lhs, spc, flags, false);
          pp_string (buffer, " =");

	  if (gimple_has_volatile_ops (gs))
	    pp_string (buffer, "{v}");

	  pp_space (buffer);
        }
      if (gimple_call_internal_p (gs))
	pp_string (buffer, internal_fn_name (gimple_call_internal_fn (gs)));
      else
	print_call_name (buffer, fn, flags);
      pp_string (buffer, " (");
      dump_gimple_call_args (buffer, gs, flags);
      pp_right_paren (buffer);
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon (buffer);
    }

  if (gimple_call_chain (gs))
    {
      pp_string (buffer, " [static-chain: ");
      dump_generic_node (buffer, gimple_call_chain (gs), spc, flags, false);
      pp_right_bracket (buffer);
    }

  if (gimple_call_return_slot_opt_p (gs))
    pp_string (buffer, " [return slot optimization]");
  if (gimple_call_tail_p (gs))
    pp_string (buffer, " [tail call]");
  if (gimple_call_must_tail_p (gs))
    pp_string (buffer, " [must tail call]");

  if (fn == NULL)
    return;

  /* Dump the arguments of _ITM_beginTransaction sanely.  */
  if (TREE_CODE (fn) == ADDR_EXPR)
    fn = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fn) == FUNCTION_DECL && decl_is_tm_clone (fn))
    pp_string (buffer, " [tm-clone]");
  if (TREE_CODE (fn) == FUNCTION_DECL
      && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fn) == BUILT_IN_TM_START
      && gimple_call_num_args (gs) > 0)
    {
      tree t = gimple_call_arg (gs, 0);
      unsigned HOST_WIDE_INT props;
      gcc_assert (TREE_CODE (t) == INTEGER_CST);

      pp_string (buffer, " [ ");

      /* Get the transaction code properties.  */
      props = TREE_INT_CST_LOW (t);

      if (props & PR_INSTRUMENTEDCODE)
	pp_string (buffer, "instrumentedCode ");
      if (props & PR_UNINSTRUMENTEDCODE)
	pp_string (buffer, "uninstrumentedCode ");
      if (props & PR_HASNOXMMUPDATE)
	pp_string (buffer, "hasNoXMMUpdate ");
      if (props & PR_HASNOABORT)
	pp_string (buffer, "hasNoAbort ");
      if (props & PR_HASNOIRREVOCABLE)
	pp_string (buffer, "hasNoIrrevocable ");
      if (props & PR_DOESGOIRREVOCABLE)
	pp_string (buffer, "doesGoIrrevocable ");
      if (props & PR_HASNOSIMPLEREADS)
	pp_string (buffer, "hasNoSimpleReads ");
      if (props & PR_AWBARRIERSOMITTED)
	pp_string (buffer, "awBarriersOmitted ");
      if (props & PR_RARBARRIERSOMITTED)
	pp_string (buffer, "RaRBarriersOmitted ");
      if (props & PR_UNDOLOGCODE)
	pp_string (buffer, "undoLogCode ");
      if (props & PR_PREFERUNINSTRUMENTED)
	pp_string (buffer, "preferUninstrumented ");
      if (props & PR_EXCEPTIONBLOCK)
	pp_string (buffer, "exceptionBlock ");
      if (props & PR_HASELSE)
	pp_string (buffer, "hasElse ");
      if (props & PR_READONLY)
	pp_string (buffer, "readOnly ");

      pp_right_bracket (buffer);
    }
}


/* Dump the switch statement GS.  BUFFER, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_switch (pretty_printer *buffer, gswitch *gs, int spc,
		    dump_flags_t flags)
{
  unsigned int i;

  GIMPLE_CHECK (gs, GIMPLE_SWITCH);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T, ", gs,
                   gimple_switch_index (gs));
  else
    {
      pp_string (buffer, "switch (");
      dump_generic_node (buffer, gimple_switch_index (gs), spc, flags, true);
      if (flags & TDF_GIMPLE)
	pp_string (buffer, ") {");
      else
	pp_string (buffer, ") <");
    }

  for (i = 0; i < gimple_switch_num_labels (gs); i++)
    {
      tree case_label = gimple_switch_label (gs, i);
      gcc_checking_assert (case_label != NULL_TREE);
      dump_generic_node (buffer, case_label, spc, flags, false);
      pp_space (buffer);
      tree label = CASE_LABEL (case_label);
      dump_generic_node (buffer, label, spc, flags, false);

      if (cfun && cfun->cfg)
	{
	  basic_block dest = label_to_block (label);
	  if (dest)
	    {
	      edge label_edge = find_edge (gimple_bb (gs), dest);
	      if (label_edge && !(flags & TDF_GIMPLE))
		dump_edge_probability (buffer, label_edge);
	    }
	}

      if (i < gimple_switch_num_labels (gs) - 1)
	{
	  if (flags & TDF_GIMPLE)
	    pp_string (buffer, "; ");
	  else
	    pp_string (buffer, ", ");
	}
    }
  if (flags & TDF_GIMPLE)
    pp_string (buffer, "; }");
  else
    pp_greater (buffer);
}


/* Dump the gimple conditional GS.  BUFFER, SPC and FLAGS are as in
   pp_gimple_stmt_1.  */

static void
dump_gimple_cond (pretty_printer *buffer, gcond *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%s, %T, %T, %T, %T>", gs,
		     get_tree_code_name (gimple_cond_code (gs)),
		     gimple_cond_lhs (gs), gimple_cond_rhs (gs),
		     gimple_cond_true_label (gs), gimple_cond_false_label (gs));
  else
    {
      if (!(flags & TDF_RHS_ONLY))
	pp_string (buffer, "if (");
      dump_generic_node (buffer, gimple_cond_lhs (gs), spc, flags, false);
      pp_space (buffer);
      pp_string (buffer, op_symbol_code (gimple_cond_code (gs)));
      pp_space (buffer);
      dump_generic_node (buffer, gimple_cond_rhs (gs), spc, flags, false);
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

	  pp_right_paren (buffer);

	  if (gimple_cond_true_label (gs))
	    {
	      pp_string (buffer, " goto ");
	      dump_generic_node (buffer, gimple_cond_true_label (gs),
				 spc, flags, false);
	      if (has_edge_info && !(flags & TDF_GIMPLE))
		dump_edge_probability (buffer, true_edge);
	      pp_semicolon (buffer);
	    }
	  if (gimple_cond_false_label (gs))
	    {
	      pp_string (buffer, " else goto ");
	      dump_generic_node (buffer, gimple_cond_false_label (gs),
				 spc, flags, false);
	      if (has_edge_info && !(flags & TDF_GIMPLE))
		dump_edge_probability (buffer, false_edge);

	      pp_semicolon (buffer);
	    }
	}
    }
}


/* Dump a GIMPLE_LABEL tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfils.h).  */

static void
dump_gimple_label (pretty_printer *buffer, glabel *gs, int spc,
		   dump_flags_t flags)
{
  tree label = gimple_label_label (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs, label);
  else
    {
      dump_generic_node (buffer, label, spc, flags, false);
      pp_colon (buffer);
    }
  if (flags & TDF_GIMPLE)
    return;
  if (DECL_NONLOCAL (label))
    pp_string (buffer, " [non-local]");
  if ((flags & TDF_EH) && EH_LANDING_PAD_NR (label))
    pp_printf (buffer, " [LP %d]", EH_LANDING_PAD_NR (label));
}

/* Dump a GIMPLE_GOTO tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  */

static void
dump_gimple_goto (pretty_printer *buffer, ggoto *gs, int spc,
		  dump_flags_t flags)
{
  tree label = gimple_goto_dest (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs, label);
  else
    dump_gimple_fmt (buffer, spc, flags, "goto %T;", label);
}


/* Dump a GIMPLE_BIND tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  */

static void
dump_gimple_bind (pretty_printer *buffer, gbind *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <", gs);
  else
    pp_left_brace (buffer);
  if (!(flags & TDF_SLIM))
    {
      tree var;

      for (var = gimple_bind_vars (gs); var; var = DECL_CHAIN (var))
	{
          newline_and_indent (buffer, 2);
	  print_declaration (buffer, var, spc, flags);
	}
      if (gimple_bind_vars (gs))
	pp_newline (buffer);
    }
  pp_newline (buffer);
  dump_gimple_seq (buffer, gimple_bind_body (gs), spc + 2, flags);
  newline_and_indent (buffer, spc);
  if (flags & TDF_RAW)
    pp_greater (buffer);
  else
    pp_right_brace (buffer);
}


/* Dump a GIMPLE_TRY tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_try (pretty_printer *buffer, gtry *gs, int spc,
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
      dump_gimple_fmt (buffer, spc, flags,
                       "%G <%s,%+EVAL <%S>%nCLEANUP <%S>%->", gs, type,
                       gimple_try_eval (gs), gimple_try_cleanup (gs));
    }
  else
    {
      pp_string (buffer, "try");
      newline_and_indent (buffer, spc + 2);
      pp_left_brace (buffer);
      pp_newline (buffer);

      dump_gimple_seq (buffer, gimple_try_eval (gs), spc + 4, flags);
      newline_and_indent (buffer, spc + 2);
      pp_right_brace (buffer);

      if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
	{
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "catch");
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	}
      else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
	{
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "finally");
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	}
      else
	pp_string (buffer, " <UNKNOWN GIMPLE_TRY> {");

      pp_newline (buffer);
      dump_gimple_seq (buffer, gimple_try_cleanup (gs), spc + 4, flags);
      newline_and_indent (buffer, spc + 2);
      pp_right_brace (buffer);
    }
}


/* Dump a GIMPLE_CATCH tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_catch (pretty_printer *buffer, gcatch *gs, int spc,
		   dump_flags_t flags)
{
  if (flags & TDF_RAW)
      dump_gimple_fmt (buffer, spc, flags, "%G <%T, %+CATCH <%S>%->", gs,
                       gimple_catch_types (gs), gimple_catch_handler (gs));
  else
      dump_gimple_fmt (buffer, spc, flags, "catch (%T)%+{%S}",
                       gimple_catch_types (gs), gimple_catch_handler (gs));
}


/* Dump a GIMPLE_EH_FILTER tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_eh_filter (pretty_printer *buffer, geh_filter *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T, %+FAILURE <%S>%->", gs,
                     gimple_eh_filter_types (gs),
                     gimple_eh_filter_failure (gs));
  else
    dump_gimple_fmt (buffer, spc, flags, "<<<eh_filter (%T)>>>%+{%+%S%-}",
                     gimple_eh_filter_types (gs),
                     gimple_eh_filter_failure (gs));
}


/* Dump a GIMPLE_EH_MUST_NOT_THROW tuple.  */

static void
dump_gimple_eh_must_not_throw (pretty_printer *buffer,
			       geh_mnt *gs, int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs,
		     gimple_eh_must_not_throw_fndecl (gs));
  else
    dump_gimple_fmt (buffer, spc, flags, "<<<eh_must_not_throw (%T)>>>",
		     gimple_eh_must_not_throw_fndecl (gs));
}


/* Dump a GIMPLE_EH_ELSE tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_eh_else (pretty_printer *buffer, geh_else *gs, int spc,
		     dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags,
		     "%G <%+N_BODY <%S>%nE_BODY <%S>%->", gs,
		     gimple_eh_else_n_body (gs), gimple_eh_else_e_body (gs));
  else
    dump_gimple_fmt (buffer, spc, flags,
		    "<<<if_normal_exit>>>%+{%S}%-<<<else_eh_exit>>>%+{%S}",
		     gimple_eh_else_n_body (gs), gimple_eh_else_e_body (gs));
}


/* Dump a GIMPLE_RESX tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_resx (pretty_printer *buffer, gresx *gs, int spc,
		  dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%d>", gs,
		     gimple_resx_region (gs));
  else
    dump_gimple_fmt (buffer, spc, flags, "resx %d", gimple_resx_region (gs));
}

/* Dump a GIMPLE_EH_DISPATCH tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_eh_dispatch (pretty_printer *buffer, geh_dispatch *gs, int spc,
			 dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%d>", gs,
		     gimple_eh_dispatch_region (gs));
  else
    dump_gimple_fmt (buffer, spc, flags, "eh_dispatch %d",
		     gimple_eh_dispatch_region (gs));
}

/* Dump a GIMPLE_DEBUG tuple on the pretty_printer BUFFER, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_debug (pretty_printer *buffer, gdebug *gs, int spc,
		   dump_flags_t flags)
{
  switch (gs->subcode)
    {
    case GIMPLE_DEBUG_BIND:
      if (flags & TDF_RAW)
	dump_gimple_fmt (buffer, spc, flags, "%G BIND <%T, %T>", gs,
			 gimple_debug_bind_get_var (gs),
			 gimple_debug_bind_get_value (gs));
      else
	dump_gimple_fmt (buffer, spc, flags, "# DEBUG %T => %T",
			 gimple_debug_bind_get_var (gs),
			 gimple_debug_bind_get_value (gs));
      break;

    case GIMPLE_DEBUG_SOURCE_BIND:
      if (flags & TDF_RAW)
	dump_gimple_fmt (buffer, spc, flags, "%G SRCBIND <%T, %T>", gs,
			 gimple_debug_source_bind_get_var (gs),
			 gimple_debug_source_bind_get_value (gs));
      else
	dump_gimple_fmt (buffer, spc, flags, "# DEBUG %T s=> %T",
			 gimple_debug_source_bind_get_var (gs),
			 gimple_debug_source_bind_get_value (gs));
      break;

    default:
      gcc_unreachable ();
    }
}

/* Dump a GIMPLE_OMP_FOR tuple on the pretty_printer BUFFER.  */
static void
dump_gimple_omp_for (pretty_printer *buffer, gomp_for *gs, int spc,
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
	case GF_OMP_FOR_KIND_CILKFOR:
	  kind = " _Cilk_for";
	  break;
	case GF_OMP_FOR_KIND_OACC_LOOP:
	  kind = " oacc_loop";
	  break;
	case GF_OMP_FOR_KIND_SIMD:
	  kind = " simd";
	  break;
	case GF_OMP_FOR_KIND_CILKSIMD:
	  kind = " cilksimd";
	  break;
	default:
	  gcc_unreachable ();
	}
      dump_gimple_fmt (buffer, spc, flags, "%G%s <%+BODY <%S>%nCLAUSES <", gs,
		       kind, gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_for_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >,");
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	dump_gimple_fmt (buffer, spc, flags,
			 "%+%T, %T, %T, %s, %T,%n",
			 gimple_omp_for_index (gs, i),
			 gimple_omp_for_initial (gs, i),
			 gimple_omp_for_final (gs, i),
			 get_tree_code_name (gimple_omp_for_cond (gs, i)),
			 gimple_omp_for_incr (gs, i));
      dump_gimple_fmt (buffer, spc, flags, "PRE_BODY <%S>%->",
		       gimple_omp_for_pre_body (gs));
    }
  else
    {
      switch (gimple_omp_for_kind (gs))
	{
	case GF_OMP_FOR_KIND_FOR:
	  pp_string (buffer, "#pragma omp for");
	  break;
	case GF_OMP_FOR_KIND_DISTRIBUTE:
	  pp_string (buffer, "#pragma omp distribute");
	  break;
	case GF_OMP_FOR_KIND_TASKLOOP:
	  pp_string (buffer, "#pragma omp taskloop");
	  break;
	case GF_OMP_FOR_KIND_CILKFOR:
	  break;
	case GF_OMP_FOR_KIND_OACC_LOOP:
	  pp_string (buffer, "#pragma acc loop");
	  break;
	case GF_OMP_FOR_KIND_SIMD:
	  pp_string (buffer, "#pragma omp simd");
	  break;
	case GF_OMP_FOR_KIND_CILKSIMD:
	  pp_string (buffer, "#pragma simd");
	  break;
	case GF_OMP_FOR_KIND_GRID_LOOP:
	  pp_string (buffer, "#pragma omp for grid_loop");
	  break;
	default:
	  gcc_unreachable ();
	}
      if (gimple_omp_for_kind (gs) != GF_OMP_FOR_KIND_CILKFOR)
	dump_omp_clauses (buffer, gimple_omp_for_clauses (gs), spc, flags);
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	{
	  if (i)
	    spc += 2;
	  if (gimple_omp_for_kind (gs) == GF_OMP_FOR_KIND_CILKFOR)
	    pp_string (buffer, "_Cilk_for (");
	  else
	    {
	      newline_and_indent (buffer, spc);
	      pp_string (buffer, "for (");
	    }
	  dump_generic_node (buffer, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_string (buffer, " = ");
	  dump_generic_node (buffer, gimple_omp_for_initial (gs, i), spc,
			     flags, false);
	  pp_string (buffer, "; ");

	  dump_generic_node (buffer, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_space (buffer);
	  switch (gimple_omp_for_cond (gs, i))
	    {
	    case LT_EXPR:
	      pp_less (buffer);
	      break;
	    case GT_EXPR:
	      pp_greater (buffer);
	      break;
	    case LE_EXPR:
	      pp_less_equal (buffer);
	      break;
	    case GE_EXPR:
	      pp_greater_equal (buffer);
	      break;
	    case NE_EXPR:
	      pp_string (buffer, "!=");
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  pp_space (buffer);
	  dump_generic_node (buffer, gimple_omp_for_final (gs, i), spc,
			     flags, false);
	  pp_string (buffer, "; ");

	  dump_generic_node (buffer, gimple_omp_for_index (gs, i), spc,
			     flags, false);
	  pp_string (buffer, " = ");
	  dump_generic_node (buffer, gimple_omp_for_incr (gs, i), spc,
			     flags, false);
	  pp_right_paren (buffer);
	}

      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  if (gimple_omp_for_kind (gs) == GF_OMP_FOR_KIND_CILKFOR)
	    dump_omp_clauses (buffer, gimple_omp_for_clauses (gs), spc, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
    }
}

/* Dump a GIMPLE_OMP_CONTINUE tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_continue (pretty_printer *buffer, gomp_continue *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%T, %T>", gs,
                       gimple_omp_continue_control_def (gs),
                       gimple_omp_continue_control_use (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp continue (");
      dump_generic_node (buffer, gimple_omp_continue_control_def (gs),
	  		 spc, flags, false);
      pp_comma (buffer);
      pp_space (buffer);
      dump_generic_node (buffer, gimple_omp_continue_control_use (gs),
	  		 spc, flags, false);
      pp_right_paren (buffer);
    }
}

/* Dump a GIMPLE_OMP_SINGLE tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_single (pretty_printer *buffer, gomp_single *gs,
			int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_single_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >");
    }
  else
    {
      pp_string (buffer, "#pragma omp single");
      dump_omp_clauses (buffer, gimple_omp_single_clauses (gs), spc, flags);
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

/* Dump a GIMPLE_OMP_TARGET tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_target (pretty_printer *buffer, gomp_target *gs,
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
    case GF_OMP_TARGET_KIND_OACC_DATA:
      kind = " oacc_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
      kind = " oacc_update";
      break;
    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
      kind = " oacc_enter_exit_data";
      break;
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      kind = " oacc_declare";
      break;
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      kind = " oacc_host_data";
      break;
    default:
      gcc_unreachable ();
    }
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G%s <%+BODY <%S>%nCLAUSES <", gs,
		       kind, gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_target_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >, %T, %T%n>",
		       gimple_omp_target_child_fn (gs),
		       gimple_omp_target_data_arg (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp target");
      pp_string (buffer, kind);
      dump_omp_clauses (buffer, gimple_omp_target_clauses (gs), spc, flags);
      if (gimple_omp_target_child_fn (gs))
	{
	  pp_string (buffer, " [child fn: ");
	  dump_generic_node (buffer, gimple_omp_target_child_fn (gs),
			     spc, flags, false);
	  pp_string (buffer, " (");
	  if (gimple_omp_target_data_arg (gs))
	    dump_generic_node (buffer, gimple_omp_target_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (buffer, "???");
	  pp_string (buffer, ")]");
	}
      gimple_seq body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
      else if (body)
	{
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 2, flags);
	}
    }
}

/* Dump a GIMPLE_OMP_TEAMS tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_teams (pretty_printer *buffer, gomp_teams *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_teams_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >");
    }
  else
    {
      pp_string (buffer, "#pragma omp teams");
      dump_omp_clauses (buffer, gimple_omp_teams_clauses (gs), spc, flags);
      if (!gimple_seq_empty_p (gimple_omp_body (gs)))
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '{');
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, gimple_omp_body (gs), spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '}');
	}
    }
}

/* Dump a GIMPLE_OMP_SECTIONS tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_sections (pretty_printer *buffer, gomp_sections *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
		       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_sections_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >");
    }
  else
    {
      pp_string (buffer, "#pragma omp sections");
      if (gimple_omp_sections_control (gs))
	{
	  pp_string (buffer, " <");
	  dump_generic_node (buffer, gimple_omp_sections_control (gs), spc,
			     flags, false);
	  pp_greater (buffer);
	}
      dump_omp_clauses (buffer, gimple_omp_sections_clauses (gs), spc, flags);
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

/* Dump a GIMPLE_OMP_{MASTER,TASKGROUP,ORDERED,SECTION} tuple on the
   pretty_printer BUFFER.  */

static void
dump_gimple_omp_block (pretty_printer *buffer, gimple *gs, int spc,
		       dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      switch (gimple_code (gs))
	{
	case GIMPLE_OMP_MASTER:
	  pp_string (buffer, "#pragma omp master");
	  break;
	case GIMPLE_OMP_TASKGROUP:
	  pp_string (buffer, "#pragma omp taskgroup");
	  break;
	case GIMPLE_OMP_SECTION:
	  pp_string (buffer, "#pragma omp section");
	  break;
	case GIMPLE_OMP_GRID_BODY:
	  pp_string (buffer, "#pragma omp gridified body");
	  break;
	default:
	  gcc_unreachable ();
	}
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

/* Dump a GIMPLE_OMP_CRITICAL tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_critical (pretty_printer *buffer, gomp_critical *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      pp_string (buffer, "#pragma omp critical");
      if (gimple_omp_critical_name (gs))
	{
	  pp_string (buffer, " (");
	  dump_generic_node (buffer, gimple_omp_critical_name (gs), spc,
			     flags, false);
	  pp_right_paren (buffer);
	}
      dump_omp_clauses (buffer, gimple_omp_critical_clauses (gs), spc, flags);
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

/* Dump a GIMPLE_OMP_ORDERED tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_ordered (pretty_printer *buffer, gomp_ordered *gs,
			 int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S> >", gs,
		     gimple_omp_body (gs));
  else
    {
      pp_string (buffer, "#pragma omp ordered");
      dump_omp_clauses (buffer, gimple_omp_ordered_clauses (gs), spc, flags);
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

/* Dump a GIMPLE_OMP_RETURN tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_return (pretty_printer *buffer, gimple *gs, int spc,
			dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <nowait=%d", gs,
                       (int) gimple_omp_return_nowait_p (gs));
      if (gimple_omp_return_lhs (gs))
	dump_gimple_fmt (buffer, spc, flags, ", lhs=%T>",
			 gimple_omp_return_lhs (gs));
      else
	dump_gimple_fmt (buffer, spc, flags, ">");
    }
  else
    {
      pp_string (buffer, "#pragma omp return");
      if (gimple_omp_return_nowait_p (gs))
	pp_string (buffer, "(nowait)");
      if (gimple_omp_return_lhs (gs))
	{
	  pp_string (buffer, " (set ");
	  dump_generic_node (buffer, gimple_omp_return_lhs (gs),
			     spc, flags, false);
	  pp_character (buffer, ')');
	}
    }
}

/* Dump a GIMPLE_TRANSACTION tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_transaction (pretty_printer *buffer, gtransaction *gs,
			 int spc, dump_flags_t flags)
{
  unsigned subcode = gimple_transaction_subcode (gs);

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags,
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
	pp_string (buffer, "__transaction_atomic [[outer]]");
      else if (subcode & GTMA_IS_RELAXED)
	pp_string (buffer, "__transaction_relaxed");
      else
	pp_string (buffer, "__transaction_atomic");
      subcode &= ~GTMA_DECLARATION_MASK;

      if (gimple_transaction_body (gs))
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, gimple_transaction_body (gs),
			   spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
      else
	{
	  pp_string (buffer, "  //");
	  if (gimple_transaction_label_norm (gs))
	    {
	      pp_string (buffer, " NORM=");
	      dump_generic_node (buffer, gimple_transaction_label_norm (gs),
				 spc, flags, false);
	    }
	  if (gimple_transaction_label_uninst (gs))
	    {
	      pp_string (buffer, " UNINST=");
	      dump_generic_node (buffer, gimple_transaction_label_uninst (gs),
				 spc, flags, false);
	    }
	  if (gimple_transaction_label_over (gs))
	    {
	      pp_string (buffer, " OVER=");
	      dump_generic_node (buffer, gimple_transaction_label_over (gs),
				 spc, flags, false);
	    }
	  if (subcode)
	    {
	      pp_string (buffer, " SUBCODE=[ ");
	      if (subcode & GTMA_HAVE_ABORT)
		{
		  pp_string (buffer, "GTMA_HAVE_ABORT ");
		  subcode &= ~GTMA_HAVE_ABORT;
		}
	      if (subcode & GTMA_HAVE_LOAD)
		{
		  pp_string (buffer, "GTMA_HAVE_LOAD ");
		  subcode &= ~GTMA_HAVE_LOAD;
		}
	      if (subcode & GTMA_HAVE_STORE)
		{
		  pp_string (buffer, "GTMA_HAVE_STORE ");
		  subcode &= ~GTMA_HAVE_STORE;
		}
	      if (subcode & GTMA_MAY_ENTER_IRREVOCABLE)
		{
		  pp_string (buffer, "GTMA_MAY_ENTER_IRREVOCABLE ");
		  subcode &= ~GTMA_MAY_ENTER_IRREVOCABLE;
		}
	      if (subcode & GTMA_DOES_GO_IRREVOCABLE)
		{
		  pp_string (buffer, "GTMA_DOES_GO_IRREVOCABLE ");
		  subcode &= ~GTMA_DOES_GO_IRREVOCABLE;
		}
	      if (subcode & GTMA_HAS_NO_INSTRUMENTATION)
		{
		  pp_string (buffer, "GTMA_HAS_NO_INSTRUMENTATION ");
		  subcode &= ~GTMA_HAS_NO_INSTRUMENTATION;
		}
	      if (subcode)
		pp_printf (buffer, "0x%x ", subcode);
	      pp_right_bracket (buffer);
	    }
	}
    }
}

/* Dump a GIMPLE_ASM tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_asm (pretty_printer *buffer, gasm *gs, int spc, dump_flags_t flags)
{
  unsigned int i, n, f, fields;

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+STRING <%n%s%n>", gs,
                       gimple_asm_string (gs));

      n = gimple_asm_noutputs (gs);
      if (n)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_string (buffer, "OUTPUT: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (buffer, gimple_asm_output_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (buffer, ", ");
	    }
	}

      n = gimple_asm_ninputs (gs);
      if (n)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_string (buffer, "INPUT: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (buffer, gimple_asm_input_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (buffer, ", ");
	    }
	}

      n = gimple_asm_nclobbers (gs);
      if (n)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_string (buffer, "CLOBBER: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (buffer, gimple_asm_clobber_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (buffer, ", ");
	    }
	}

      n = gimple_asm_nlabels (gs);
      if (n)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_string (buffer, "LABEL: ");
	  for (i = 0; i < n; i++)
	    {
	      dump_generic_node (buffer, gimple_asm_label_op (gs, i),
				 spc, flags, false);
	      if (i < n - 1)
		pp_string (buffer, ", ");
	    }
	}

      newline_and_indent (buffer, spc);
      pp_greater (buffer);
    }
  else
    {
      pp_string (buffer, "__asm__");
      if (gimple_asm_volatile_p (gs))
	pp_string (buffer, " __volatile__");
      if (gimple_asm_nlabels (gs))
	pp_string (buffer, " goto");
      pp_string (buffer, "(\"");
      pp_string (buffer, gimple_asm_string (gs));
      pp_string (buffer, "\"");

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
	  pp_string (buffer, " : ");

	  switch (f)
	    {
	    case 0:
	      n = gimple_asm_noutputs (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (buffer, gimple_asm_output_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (buffer, ", ");
		}
	      break;

	    case 1:
	      n = gimple_asm_ninputs (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (buffer, gimple_asm_input_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (buffer, ", ");
		}
	      break;

	    case 2:
	      n = gimple_asm_nclobbers (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (buffer, gimple_asm_clobber_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (buffer, ", ");
		}
	      break;

	    case 3:
	      n = gimple_asm_nlabels (gs);
	      for (i = 0; i < n; i++)
		{
		  dump_generic_node (buffer, gimple_asm_label_op (gs, i),
				     spc, flags, false);
		  if (i < n - 1)
		    pp_string (buffer, ", ");
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      pp_string (buffer, ");");
    }
}

/* Dump ptr_info and range_info for NODE on pretty_printer BUFFER with
   SPC spaces of indent.  */

static void
dump_ssaname_info (pretty_printer *buffer, tree node, int spc)
{
  if (TREE_CODE (node) != SSA_NAME)
    return;

  if (POINTER_TYPE_P (TREE_TYPE (node))
      && SSA_NAME_PTR_INFO (node))
    {
      unsigned int align, misalign;
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (node);
      pp_string (buffer, "# PT = ");
      pp_points_to_solution (buffer, &pi->pt);
      newline_and_indent (buffer, spc);
      if (get_ptr_info_alignment (pi, &align, &misalign))
	{
	  pp_printf (buffer, "# ALIGN = %u, MISALIGN = %u", align, misalign);
	  newline_and_indent (buffer, spc);
	}
    }

  if (!POINTER_TYPE_P (TREE_TYPE (node))
      && SSA_NAME_RANGE_INFO (node))
    {
      wide_int min, max, nonzero_bits;
      value_range_type range_type = get_range_info (node, &min, &max);

      if (range_type == VR_VARYING)
	pp_printf (buffer, "# RANGE VR_VARYING");
      else if (range_type == VR_RANGE || range_type == VR_ANTI_RANGE)
	{
	  pp_printf (buffer, "# RANGE ");
	  pp_printf (buffer, "%s[", range_type == VR_RANGE ? "" : "~");
	  pp_wide_int (buffer, min, TYPE_SIGN (TREE_TYPE (node)));
	  pp_printf (buffer, ", ");
	  pp_wide_int (buffer, max, TYPE_SIGN (TREE_TYPE (node)));
	  pp_printf (buffer, "]");
	}
      nonzero_bits = get_nonzero_bits (node);
      if (nonzero_bits != -1)
	{
	  pp_string (buffer, " NONZERO ");
	  pp_wide_int (buffer, nonzero_bits, UNSIGNED);
	}
      newline_and_indent (buffer, spc);
    }
}

/* As dump_ssaname_info, but dump to FILE.  */

void
dump_ssaname_info_to_file (FILE *file, tree node, int spc)
{
  pretty_printer buffer;
  pp_needs_newline (&buffer) = true;
  buffer.buffer->stream = file;
  dump_ssaname_info (&buffer, node, spc);
  pp_flush (&buffer);
}

/* Dump a PHI node PHI.  BUFFER, SPC and FLAGS are as in pp_gimple_stmt_1.
   The caller is responsible for calling pp_flush on BUFFER to finalize
   pretty printer.  If COMMENT is true, print this after #.  */

static void
dump_gimple_phi (pretty_printer *buffer, gphi *phi, int spc, bool comment,
		 dump_flags_t flags)
{
  size_t i;
  tree lhs = gimple_phi_result (phi);

  if (flags & TDF_ALIAS)
    dump_ssaname_info (buffer, lhs, spc);

  if (comment)
    pp_string (buffer, "# ");

  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T, ", phi,
		     gimple_phi_result (phi));
  else
    {
      dump_generic_node (buffer, lhs, spc, flags, false);
      if (flags & TDF_GIMPLE)
	pp_string (buffer, " = __PHI (");
      else
	pp_string (buffer, " = PHI <");
    }
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      if ((flags & TDF_LINENO) && gimple_phi_arg_has_location (phi, i))
	dump_location (buffer, gimple_phi_arg_location (phi, i));
      if (flags & TDF_GIMPLE)
	{
	  basic_block src = gimple_phi_arg_edge (phi, i)->src;
	  gimple *stmt = first_stmt (src);
	  if (!stmt || gimple_code (stmt) != GIMPLE_LABEL)
	    {
	      pp_string (buffer, "bb_");
	      pp_decimal_int (buffer, src->index);
	    }
	  else
	    dump_generic_node (buffer, gimple_label_label (as_a <glabel *> (stmt)), 0, flags,
			       false);
	  pp_string (buffer, ": ");
	}
      dump_generic_node (buffer, gimple_phi_arg_def (phi, i), spc, flags,
			 false);
      if (! (flags & TDF_GIMPLE))
	{
	  pp_left_paren (buffer);
	  pp_decimal_int (buffer, gimple_phi_arg_edge (phi, i)->src->index);
	  pp_right_paren (buffer);
	}
      if (i < gimple_phi_num_args (phi) - 1)
	pp_string (buffer, ", ");
    }
  if (flags & TDF_GIMPLE)
    pp_string (buffer, ");");
  else
    pp_greater (buffer);
}


/* Dump a GIMPLE_OMP_PARALLEL tuple on the pretty_printer BUFFER, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_omp_parallel (pretty_printer *buffer, gomp_parallel *gs,
			  int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
                       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_parallel_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >, %T, %T%n>",
                       gimple_omp_parallel_child_fn (gs),
                       gimple_omp_parallel_data_arg (gs));
    }
  else
    {
      gimple_seq body;
      pp_string (buffer, "#pragma omp parallel");
      dump_omp_clauses (buffer, gimple_omp_parallel_clauses (gs), spc, flags);
      if (gimple_omp_parallel_child_fn (gs))
	{
	  pp_string (buffer, " [child fn: ");
	  dump_generic_node (buffer, gimple_omp_parallel_child_fn (gs),
			     spc, flags, false);
	  pp_string (buffer, " (");
	  if (gimple_omp_parallel_data_arg (gs))
	    dump_generic_node (buffer, gimple_omp_parallel_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (buffer, "???");
	  pp_string (buffer, ")]");
	}
      body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
      else if (body)
	{
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 2, flags);
	}
    }
}


/* Dump a GIMPLE_OMP_TASK tuple on the pretty_printer BUFFER, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  */

static void
dump_gimple_omp_task (pretty_printer *buffer, gomp_task *gs, int spc,
		      dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
                       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_task_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >, %T, %T, %T, %T, %T%n>",
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
	pp_string (buffer, "#pragma omp taskloop");
      else
	pp_string (buffer, "#pragma omp task");
      dump_omp_clauses (buffer, gimple_omp_task_clauses (gs), spc, flags);
      if (gimple_omp_task_child_fn (gs))
	{
	  pp_string (buffer, " [child fn: ");
	  dump_generic_node (buffer, gimple_omp_task_child_fn (gs),
			     spc, flags, false);
	  pp_string (buffer, " (");
	  if (gimple_omp_task_data_arg (gs))
	    dump_generic_node (buffer, gimple_omp_task_data_arg (gs),
			       spc, flags, false);
	  else
	    pp_string (buffer, "???");
	  pp_string (buffer, ")]");
	}
      body = gimple_omp_body (gs);
      if (body && gimple_code (gimple_seq_first_stmt (body)) != GIMPLE_BIND)
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_left_brace (buffer);
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_right_brace (buffer);
	}
      else if (body)
	{
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 2, flags);
	}
    }
}


/* Dump a GIMPLE_OMP_ATOMIC_LOAD tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_omp_atomic_load (pretty_printer *buffer, gomp_atomic_load *gs,
			     int spc, dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%T, %T>", gs,
                       gimple_omp_atomic_load_lhs (gs),
                       gimple_omp_atomic_load_rhs (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp atomic_load");
      if (gimple_omp_atomic_seq_cst_p (gs))
	pp_string (buffer, " seq_cst");
      if (gimple_omp_atomic_need_value_p (gs))
	pp_string (buffer, " [needed]");
      newline_and_indent (buffer, spc + 2);
      dump_generic_node (buffer, gimple_omp_atomic_load_lhs (gs),
	  		 spc, flags, false);
      pp_space (buffer);
      pp_equal (buffer);
      pp_space (buffer);
      pp_star (buffer);
      dump_generic_node (buffer, gimple_omp_atomic_load_rhs (gs),
	  		 spc, flags, false);
    }
}

/* Dump a GIMPLE_OMP_ATOMIC_STORE tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in dumpfile.h).  */

static void
dump_gimple_omp_atomic_store (pretty_printer *buffer,
			      gomp_atomic_store *gs, int spc,
			      dump_flags_t flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs,
                       gimple_omp_atomic_store_val (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp atomic_store ");
      if (gimple_omp_atomic_seq_cst_p (gs))
	pp_string (buffer, "seq_cst ");
      if (gimple_omp_atomic_need_value_p (gs))
	pp_string (buffer, "[needed] ");
      pp_left_paren (buffer);
      dump_generic_node (buffer, gimple_omp_atomic_store_val (gs),
	  		 spc, flags, false);
      pp_right_paren (buffer);
    }
}


/* Dump all the memory operands for statement GS.  BUFFER, SPC and
   FLAGS are as in pp_gimple_stmt_1.  */

static void
dump_gimple_mem_ops (pretty_printer *buffer, gimple *gs, int spc,
		     dump_flags_t flags)
{
  tree vdef = gimple_vdef (gs);
  tree vuse = gimple_vuse (gs);

  if (vdef != NULL_TREE)
    {
      pp_string (buffer, "# ");
      dump_generic_node (buffer, vdef, spc + 2, flags, false);
      pp_string (buffer, " = VDEF <");
      dump_generic_node (buffer, vuse, spc + 2, flags, false);
      pp_greater (buffer);
      newline_and_indent (buffer, spc);
    }
  else if (vuse != NULL_TREE)
    {
      pp_string (buffer, "# VUSE <");
      dump_generic_node (buffer, vuse, spc + 2, flags, false);
      pp_greater (buffer);
      newline_and_indent (buffer, spc);
    }
}


/* Print the gimple statement GS on the pretty printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in dumpfile.h).  The caller is responsible for calling
   pp_flush on BUFFER to finalize the pretty printer.  */

void
pp_gimple_stmt_1 (pretty_printer *buffer, gimple *gs, int spc,
		  dump_flags_t flags)
{
  if (!gs)
    return;

  if (flags & TDF_STMTADDR)
    pp_printf (buffer, "<&%p> ", (void *) gs);

  if ((flags & TDF_LINENO) && gimple_has_location (gs))
    dump_location (buffer, gimple_location (gs));

  if (flags & TDF_EH)
    {
      int lp_nr = lookup_stmt_eh_lp (gs);
      if (lp_nr > 0)
	pp_printf (buffer, "[LP %d] ", lp_nr);
      else if (lp_nr < 0)
	pp_printf (buffer, "[MNT %d] ", -lp_nr);
    }

  if ((flags & (TDF_VOPS|TDF_MEMSYMS))
      && gimple_has_mem_ops (gs))
    dump_gimple_mem_ops (buffer, gs, spc, flags);

  if (gimple_has_lhs (gs)
      && (flags & TDF_ALIAS))
    dump_ssaname_info (buffer, gimple_get_lhs (gs), spc);

  switch (gimple_code (gs))
    {
    case GIMPLE_ASM:
      dump_gimple_asm (buffer, as_a <gasm *> (gs), spc, flags);
      break;

    case GIMPLE_ASSIGN:
      dump_gimple_assign (buffer, as_a <gassign *> (gs), spc, flags);
      break;

    case GIMPLE_BIND:
      dump_gimple_bind (buffer, as_a <gbind *> (gs), spc, flags);
      break;

    case GIMPLE_CALL:
      dump_gimple_call (buffer, as_a <gcall *> (gs), spc, flags);
      break;

    case GIMPLE_COND:
      dump_gimple_cond (buffer, as_a <gcond *> (gs), spc, flags);
      break;

    case GIMPLE_LABEL:
      dump_gimple_label (buffer, as_a <glabel *> (gs), spc, flags);
      break;

    case GIMPLE_GOTO:
      dump_gimple_goto (buffer, as_a <ggoto *> (gs), spc, flags);
      break;

    case GIMPLE_NOP:
      pp_string (buffer, "GIMPLE_NOP");
      break;

    case GIMPLE_RETURN:
      dump_gimple_return (buffer, as_a <greturn *> (gs), spc, flags);
      break;

    case GIMPLE_SWITCH:
      dump_gimple_switch (buffer, as_a <gswitch *> (gs), spc, flags);
      break;

    case GIMPLE_TRY:
      dump_gimple_try (buffer, as_a <gtry *> (gs), spc, flags);
      break;

    case GIMPLE_PHI:
      dump_gimple_phi (buffer, as_a <gphi *> (gs), spc, false, flags);
      break;

    case GIMPLE_OMP_PARALLEL:
      dump_gimple_omp_parallel (buffer, as_a <gomp_parallel *> (gs), spc,
				flags);
      break;

    case GIMPLE_OMP_TASK:
      dump_gimple_omp_task (buffer, as_a <gomp_task *> (gs), spc, flags);
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
      dump_gimple_omp_atomic_load (buffer, as_a <gomp_atomic_load *> (gs),
				   spc, flags);
      break;

    case GIMPLE_OMP_ATOMIC_STORE:
      dump_gimple_omp_atomic_store (buffer,
				    as_a <gomp_atomic_store *> (gs),
				    spc, flags);
      break;

    case GIMPLE_OMP_FOR:
      dump_gimple_omp_for (buffer, as_a <gomp_for *> (gs), spc, flags);
      break;

    case GIMPLE_OMP_CONTINUE:
      dump_gimple_omp_continue (buffer, as_a <gomp_continue *> (gs), spc,
				flags);
      break;

    case GIMPLE_OMP_SINGLE:
      dump_gimple_omp_single (buffer, as_a <gomp_single *> (gs), spc,
			      flags);
      break;

    case GIMPLE_OMP_TARGET:
      dump_gimple_omp_target (buffer, as_a <gomp_target *> (gs), spc,
			      flags);
      break;

    case GIMPLE_OMP_TEAMS:
      dump_gimple_omp_teams (buffer, as_a <gomp_teams *> (gs), spc,
			     flags);
      break;

    case GIMPLE_OMP_RETURN:
      dump_gimple_omp_return (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS:
      dump_gimple_omp_sections (buffer, as_a <gomp_sections *> (gs),
				spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS_SWITCH:
      pp_string (buffer, "GIMPLE_SECTIONS_SWITCH");
      break;

    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_GRID_BODY:
      dump_gimple_omp_block (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_ORDERED:
      dump_gimple_omp_ordered (buffer, as_a <gomp_ordered *> (gs), spc,
			       flags);
      break;

    case GIMPLE_OMP_CRITICAL:
      dump_gimple_omp_critical (buffer, as_a <gomp_critical *> (gs), spc,
				flags);
      break;

    case GIMPLE_CATCH:
      dump_gimple_catch (buffer, as_a <gcatch *> (gs), spc, flags);
      break;

    case GIMPLE_EH_FILTER:
      dump_gimple_eh_filter (buffer, as_a <geh_filter *> (gs), spc, flags);
      break;

    case GIMPLE_EH_MUST_NOT_THROW:
      dump_gimple_eh_must_not_throw (buffer,
				     as_a <geh_mnt *> (gs),
				     spc, flags);
      break;

    case GIMPLE_EH_ELSE:
      dump_gimple_eh_else (buffer, as_a <geh_else *> (gs), spc, flags);
      break;

    case GIMPLE_RESX:
      dump_gimple_resx (buffer, as_a <gresx *> (gs), spc, flags);
      break;

    case GIMPLE_EH_DISPATCH:
      dump_gimple_eh_dispatch (buffer, as_a <geh_dispatch *> (gs), spc,
			       flags);
      break;

    case GIMPLE_DEBUG:
      dump_gimple_debug (buffer, as_a <gdebug *> (gs), spc, flags);
      break;

    case GIMPLE_PREDICT:
      pp_string (buffer, "// predicted ");
      if (gimple_predict_outcome (gs))
	pp_string (buffer, "likely by ");
      else
	pp_string (buffer, "unlikely by ");
      pp_string (buffer, predictor_name (gimple_predict_predictor (gs)));
      pp_string (buffer, " predictor.");
      break;

    case GIMPLE_TRANSACTION:
      dump_gimple_transaction (buffer, as_a <gtransaction *> (gs), spc,
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
	  if (bb->discriminator)
	    fprintf (outf, ", discriminator %i", bb->discriminator);
	  fputc ('\n', outf);
	}
    }
  else
    {
      if (flags & TDF_GIMPLE)
	fprintf (outf, "%*sbb_%d:\n", indent, "", bb->index);
      else
	fprintf (outf, "%*s<bb %d> %s:\n",
		 indent, "", bb->index, dump_profile (bb->frequency,
						      bb->count));
    }
}


/* Dumps end of basic block BB to buffer BUFFER indented by INDENT
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


/* Dump PHI nodes of basic block BB to BUFFER with details described
   by FLAGS and indented by INDENT spaces.  */

static void
dump_phi_nodes (pretty_printer *buffer, basic_block bb, int indent,
		dump_flags_t flags)
{
  gphi_iterator i;

  for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
    {
      gphi *phi = i.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)) || (flags & TDF_VOPS))
        {
          INDENT (indent);
	  dump_gimple_phi (buffer, phi, indent,
			   (flags & TDF_GIMPLE) ? false : true, flags);
          pp_newline (buffer);
        }
    }
}


/* Dump jump to basic block BB that is represented implicitly in the cfg
   to BUFFER.  */

static void
pp_cfg_jump (pretty_printer *buffer, edge e, dump_flags_t flags)
{
  if (flags & TDF_GIMPLE)
    {
      pp_string (buffer, "goto bb_");
      pp_decimal_int (buffer, e->dest->index);
      pp_semicolon (buffer);
    }
  else
    {
      pp_string (buffer, "goto <bb ");
      pp_decimal_int (buffer, e->dest->index);
      pp_greater (buffer);
      pp_semicolon (buffer);

      dump_edge_probability (buffer, e);
    }
}


/* Dump edges represented implicitly in basic block BB to BUFFER, indented
   by INDENT spaces, with details given by FLAGS.  */

static void
dump_implicit_edges (pretty_printer *buffer, basic_block bb, int indent,
		     dump_flags_t flags)
{
  edge e;
  gimple *stmt;

  stmt = last_stmt (bb);

  if (stmt && gimple_code (stmt) == GIMPLE_COND)
    {
      edge true_edge, false_edge;

      /* When we are emitting the code or changing CFG, it is possible that
	 the edges are not yet created.  When we are using debug_bb in such
	 a situation, we do not want it to crash.  */
      if (EDGE_COUNT (bb->succs) != 2)
	return;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      INDENT (indent + 2);
      pp_cfg_jump (buffer, true_edge, flags);
      newline_and_indent (buffer, indent);
      pp_string (buffer, "else");
      newline_and_indent (buffer, indent + 2);
      pp_cfg_jump (buffer, false_edge, flags);
      pp_newline (buffer);
      return;
    }

  /* If there is a fallthru edge, we may need to add an artificial
     goto to the dump.  */
  e = find_fallthru_edge (bb->succs);

  if (e && e->dest != bb->next_bb)
    {
      INDENT (indent);

      if ((flags & TDF_LINENO)
	  && e->goto_locus != UNKNOWN_LOCATION)
	dump_location (buffer, e->goto_locus);

      pp_cfg_jump (buffer, e, flags);
      pp_newline (buffer);
    }
}


/* Dumps basic block BB to buffer BUFFER with details described by FLAGS and
   indented by INDENT spaces.  */

static void
gimple_dump_bb_buff (pretty_printer *buffer, basic_block bb, int indent,
		     dump_flags_t flags)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  int label_indent = indent - 2;

  if (label_indent < 0)
    label_indent = 0;

  dump_phi_nodes (buffer, bb, indent, flags);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      int curr_indent;

      stmt = gsi_stmt (gsi);

      curr_indent = gimple_code (stmt) == GIMPLE_LABEL ? label_indent : indent;

      INDENT (curr_indent);
      pp_gimple_stmt_1 (buffer, stmt, curr_indent, flags);
      pp_newline_and_flush (buffer);
      gcc_checking_assert (DECL_STRUCT_FUNCTION (current_function_decl));
      dump_histograms_for_stmt (DECL_STRUCT_FUNCTION (current_function_decl),
				pp_buffer (buffer)->stream, stmt);
    }

  dump_implicit_edges (buffer, bb, indent, flags);
  pp_flush (buffer);
}


/* Dumps basic block BB to FILE with details described by FLAGS and
   indented by INDENT spaces.  */

void
gimple_dump_bb (FILE *file, basic_block bb, int indent, dump_flags_t flags)
{
  dump_gimple_bb_header (file, bb, indent, flags);
  if (bb->index >= NUM_FIXED_BLOCKS)
    {
      pretty_printer buffer;
      pp_needs_newline (&buffer) = true;
      buffer.buffer->stream = file;
      gimple_dump_bb_buff (&buffer, bb, indent, flags);
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


/* Handle the %G format for TEXT.  Same as %K in handle_K_format in
   tree-pretty-print.c but with a Gimple call statement as an argument.  */

void
percent_G_format (text_info *text)
{
  gcall *stmt = va_arg (*text->args_ptr, gcall*);

  /* Build a call expression from the Gimple call statement and
     pass it to the K formatter that knows how to format it.  */
  tree exp = build_vl_exp (CALL_EXPR, gimple_call_num_args (stmt) + 3);
  CALL_EXPR_FN (exp) = gimple_call_fn (stmt);
  TREE_TYPE (exp) = gimple_call_return_type (stmt);
  CALL_EXPR_STATIC_CHAIN (exp) = gimple_call_chain (stmt);
  SET_EXPR_LOCATION (exp, gimple_location (stmt));

  percent_K_format (text, exp);
}
