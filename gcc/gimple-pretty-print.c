/* Pretty formatting of GIMPLE statements and expressions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "diagnostic.h"
#include "real.h"
#include "hashtab.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "gimple.h"
#include "value-prof.h"

#define INDENT(SPACE)							\
  do { int i; for (i = 0; i < SPACE; i++) pp_space (buffer); } while (0)

static pretty_printer buffer;
static bool initialized = false;

#define GIMPLE_NIY do_niy (buffer,gs)

/* Try to print on BUFFER a default message for the unrecognized
   gimple statement GS.  */

static void
do_niy (pretty_printer *buffer, gimple gs)
{
  pp_printf (buffer, "<<< Unknown GIMPLE statement: %s >>>\n",
	     gimple_code_name[(int) gimple_code (gs)]);
}


/* Initialize the pretty printer on FILE if needed.  */

static void
maybe_init_pretty_print (FILE *file)
{
  if (!initialized)
    {
      pp_construct (&buffer, NULL, 0);
      pp_needs_newline (&buffer) = true;
      initialized = true;
    }

  buffer.buffer->stream = file;
}


/* Emit a newline and SPC indentantion spaces to BUFFER.  */

static void
newline_and_indent (pretty_printer *buffer, int spc)
{
  pp_newline (buffer);
  INDENT (spc);
}


/* Print the GIMPLE statement GS on stderr.  */

void
debug_gimple_stmt (gimple gs)
{
  print_gimple_stmt (stderr, gs, 0, TDF_VOPS|TDF_MEMSYMS);
  fprintf (stderr, "\n");
}


/* Dump GIMPLE statement G to FILE using SPC indentantion spaces and
   FLAGS as in dump_gimple_stmt.  */

void
print_gimple_stmt (FILE *file, gimple g, int spc, int flags)
{
  maybe_init_pretty_print (file);
  dump_gimple_stmt (&buffer, g, spc, flags);
  pp_flush (&buffer);
}


/* Dump GIMPLE statement G to FILE using SPC indentantion spaces and
   FLAGS as in dump_gimple_stmt.  Print only the right-hand side
   of the statement.  */

void
print_gimple_expr (FILE *file, gimple g, int spc, int flags)
{
  flags |= TDF_RHS_ONLY;
  maybe_init_pretty_print (file);
  dump_gimple_stmt (&buffer, g, spc, flags);
}


/* Print the GIMPLE sequence SEQ on BUFFER using SPC indentantion
   spaces and FLAGS as in dump_gimple_stmt.  */

static void
dump_gimple_seq (pretty_printer *buffer, gimple_seq seq, int spc, int flags)
{
  gimple_stmt_iterator i;

  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gimple gs = gsi_stmt (i);
      INDENT (spc);
      dump_gimple_stmt (buffer, gs, spc, flags);
      if (!gsi_one_before_end_p (i))
	pp_newline (buffer);
    }
}


/* Dump GIMPLE sequence SEQ to FILE using SPC indentantion spaces and
   FLAGS as in dump_gimple_stmt.  */

void
print_gimple_seq (FILE *file, gimple_seq seq, int spc, int flags)
{
  maybe_init_pretty_print (file);
  dump_gimple_seq (&buffer, seq, spc, flags);
  pp_flush (&buffer);
}


/* Print the GIMPLE sequence SEQ on stderr.  */

void
debug_gimple_seq (gimple_seq seq)
{
  print_gimple_seq (stderr, seq, 0, TDF_VOPS|TDF_MEMSYMS);
}


/* A simple helper to pretty-print some of the gimple tuples in the printf
   style. The format modifiers are preceeded by '%' and are:
     'G' - outputs a string corresponding to the code of the given gimple,
     'S' - outputs a gimple_seq with indent of spc + 2,
     'T' - outputs the tree t,
     'd' - outputs an int as a decimal,
     's' - outputs a string,
     'n' - outputs a newline,
     '+' - increases indent by 2 then outputs a newline,
     '-' - decreases indent by 2 then outputs a newline.   */

static void
dump_gimple_fmt (pretty_printer *buffer, int spc, int flags,
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
          gimple g;
          switch (*++c)
            {
              case 'G':
                g = va_arg (args, gimple);
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
   assignment GS.  BUFFER, SPC and FLAGS are as in dump_gimple_stmt.  */

static void
dump_unary_rhs (pretty_printer *buffer, gimple gs, int spc, int flags)
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
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
      pp_character (buffer, '(');
      dump_generic_node (buffer, TREE_TYPE (lhs), spc, flags, false);
      pp_string (buffer, ") ");
      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  pp_character (buffer, ')');
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
      pp_string (buffer, "ABS_EXPR <");
      dump_generic_node (buffer, rhs, spc, flags, false);
      pp_character (buffer, '>');
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
	pp_character (buffer, '~');
      else if (rhs_code == TRUTH_NOT_EXPR)
	pp_character (buffer, '!');
      else if (rhs_code == NEGATE_EXPR)
	pp_character (buffer, '-');
      else
	{
	  pp_character (buffer, '[');
	  pp_string (buffer, tree_code_name [rhs_code]);
	  pp_string (buffer, "] ");
	}

      if (op_prio (rhs) < op_code_prio (rhs_code))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, rhs, spc, flags, false);
	  pp_character (buffer, ')');
	}
      else
	dump_generic_node (buffer, rhs, spc, flags, false);
      break;
    }
}


/* Helper for dump_gimple_assign.  Print the binary RHS of the
   assignment GS.  BUFFER, SPC and FLAGS are as in dump_gimple_stmt.  */

static void
dump_binary_rhs (pretty_printer *buffer, gimple gs, int spc, int flags)
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
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_EXTRACT_EVEN_EXPR:
    case VEC_EXTRACT_ODD_EXPR:
    case VEC_INTERLEAVE_HIGH_EXPR:
    case VEC_INTERLEAVE_LOW_EXPR:
      for (p = tree_code_name [(int) code]; *p; p++)
	pp_character (buffer, TOUPPER (*p));
      pp_string (buffer, " <");
      dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
      pp_character (buffer, '>');
      break;

    default:
      if (op_prio (gimple_assign_rhs1 (gs)) <= op_code_prio (code))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags,
			     false);
	  pp_character (buffer, ')');
	}
      else
	dump_generic_node (buffer, gimple_assign_rhs1 (gs), spc, flags, false);
      pp_space (buffer);
      pp_string (buffer, op_symbol_code (gimple_assign_rhs_code (gs)));
      pp_space (buffer);
      if (op_prio (gimple_assign_rhs2 (gs)) <= op_code_prio (code))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags,
			     false);
	  pp_character (buffer, ')');
	}
      else
	dump_generic_node (buffer, gimple_assign_rhs2 (gs), spc, flags, false);
    }
}


/* Dump the gimple assignment GS.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_assign (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (flags & TDF_RAW)
    {
      tree last;
      if (gimple_num_ops (gs) == 2)
        last = NULL_TREE;
      else if (gimple_num_ops (gs) == 3)
        last = gimple_assign_rhs2 (gs);
      else
        gcc_unreachable ();

      dump_gimple_fmt (buffer, spc, flags, "%G <%s, %T, %T, %T>", gs,
                       tree_code_name[gimple_assign_rhs_code (gs)],
                       gimple_assign_lhs (gs), gimple_assign_rhs1 (gs), last);
    }
  else
    {
      if (!(flags & TDF_RHS_ONLY))
	{
	  dump_generic_node (buffer, gimple_assign_lhs (gs), spc, flags, false);
	  pp_space (buffer);
	  pp_character (buffer, '=');

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
      else
        gcc_unreachable ();
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon(buffer);
    }
}


/* Dump the return statement GS.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_return (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  tree t;

  t = gimple_return_retval (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs, t);
  else
    {
      pp_string (buffer, "return");
      if (t)
	{
	  pp_space (buffer);
	  dump_generic_node (buffer, t, spc, flags, false);
	}
      pp_semicolon (buffer);
    }
}


/* Dump the call arguments for a gimple call. BUFFER, FLAGS are as in
   dump_gimple_call.  */

static void
dump_gimple_call_args (pretty_printer *buffer, gimple gs, int flags)
{
  size_t i;

  for (i = 0; i < gimple_call_num_args (gs); i++)
    {
      dump_generic_node (buffer, gimple_call_arg (gs, i), 0, flags, false);
      if (i < gimple_call_num_args (gs) - 1)
	pp_string (buffer, ", ");
    }

  if (gimple_call_va_arg_pack_p (gs))
    {
      if (gimple_call_num_args (gs) > 0)
        {
          pp_character (buffer, ',');
          pp_space (buffer);
        }

      pp_string (buffer, "__builtin_va_arg_pack ()");
    }
}


/* Dump the call statement GS.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_call (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  tree lhs = gimple_call_lhs (gs);

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%T, %T",
                     gs, gimple_call_fn (gs), lhs);
      if (gimple_call_num_args (gs) > 0)
        {
          pp_string (buffer, ", ");
          dump_gimple_call_args (buffer, gs, flags);
        }
      pp_character (buffer, '>');
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
      print_call_name (buffer, gimple_call_fn (gs), flags);
      pp_string (buffer, " (");
      dump_gimple_call_args (buffer, gs, flags);
      pp_character (buffer, ')');
      if (!(flags & TDF_RHS_ONLY))
	pp_semicolon (buffer);
    }

  if (gimple_call_chain (gs))
    {
      pp_string (buffer, " [static-chain: ");
      dump_generic_node (buffer, gimple_call_chain (gs), spc, flags, false);
      pp_character (buffer, ']');
    }

  if (gimple_call_return_slot_opt_p (gs))
    pp_string (buffer, " [return slot optimization]");

  if (gimple_call_tail_p (gs))
    pp_string (buffer, " [tail call]");
}


/* Dump the switch statement GS.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_switch (pretty_printer *buffer, gimple gs, int spc, int flags)
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
      pp_string (buffer, ") <");
    }

  for (i = 0; i < gimple_switch_num_labels (gs); i++)
    {
      tree case_label = gimple_switch_label (gs, i);
      if (case_label == NULL_TREE)
	continue;

      dump_generic_node (buffer, case_label, spc, flags, false);
      pp_character (buffer, ' ');
      dump_generic_node (buffer, CASE_LABEL (case_label), spc, flags, false);
      if (i < gimple_switch_num_labels (gs) - 1)
        pp_string (buffer, ", ");
    }
  pp_character (buffer, '>');
}


/* Dump the gimple conditional GS.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_cond (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%s, %T, %T, %T, %T>", gs,
                   tree_code_name [gimple_cond_code (gs)],
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
	  pp_character (buffer, ')');

	  if (gimple_cond_true_label (gs))
	    {
	      pp_string (buffer, " goto ");
	      dump_generic_node (buffer, gimple_cond_true_label (gs),
				 spc, flags, false);
	      pp_semicolon (buffer);
	    }
	  if (gimple_cond_false_label (gs))
	    {
	      pp_string (buffer, " else goto ");
	      dump_generic_node (buffer, gimple_cond_false_label (gs),
				 spc, flags, false);
	      pp_semicolon (buffer);
	    }
	}
    }
}


/* Dump a GIMPLE_LABEL tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in tree-pass.h).  */

static void
dump_gimple_label (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  tree label = gimple_label_label (gs);
  if (flags & TDF_RAW)
      dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs, label);
  else
    {
      dump_generic_node (buffer, label, spc, flags, false);
      pp_character (buffer, ':');
    }
  if (DECL_NONLOCAL (label))
    pp_string (buffer, " [non-local]");
}

/* Dump a GIMPLE_GOTO tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in tree-pass.h).  */

static void
dump_gimple_goto (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  tree label = gimple_goto_dest (gs);
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs, label);
  else
    dump_gimple_fmt (buffer, spc, flags, "goto %T;", label);
}


/* Dump a GIMPLE_BIND tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in tree-pass.h).  */

static void
dump_gimple_bind (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <", gs);
  else
    pp_character (buffer, '{');
  if (!(flags & TDF_SLIM))
    {
      tree var;

      for (var = gimple_bind_vars (gs); var; var = TREE_CHAIN (var))
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
    pp_character (buffer, '>');
  else
    pp_character (buffer, '}');
}


/* Dump a GIMPLE_TRY tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  */

static void
dump_gimple_try (pretty_printer *buffer, gimple gs, int spc, int flags)
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
      pp_character (buffer, '{');
      pp_newline (buffer);

      dump_gimple_seq (buffer, gimple_try_eval (gs), spc + 4, flags);
      newline_and_indent (buffer, spc + 2);
      pp_character (buffer, '}');

      if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
	{
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "catch");
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '{');
	}
      else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
	{
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "finally");
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '{');
	}
      else
	pp_string (buffer, " <UNKNOWN GIMPLE_TRY> {");

      pp_newline (buffer);
      dump_gimple_seq (buffer, gimple_try_cleanup (gs), spc + 4, flags);
      newline_and_indent (buffer, spc + 2);
      pp_character (buffer, '}');
    }
}


/* Dump a GIMPLE_CATCH tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  */

static void
dump_gimple_catch (pretty_printer *buffer, gimple gs, int spc, int flags)
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
   tree-pass.h).  */

static void
dump_gimple_eh_filter (pretty_printer *buffer, gimple gs, int spc, int flags)
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


/* Dump a GIMPLE_RESX tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  */

static void
dump_gimple_resx (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%d>", gs,
                     gimple_resx_region (gs));
  else
    dump_gimple_fmt (buffer, spc, flags, "resx %d", gimple_resx_region (gs));
}

/* Dump a GIMPLE_OMP_FOR tuple on the pretty_printer BUFFER.  */
static void
dump_gimple_omp_for (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  size_t i;

  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%+BODY <%S>%nCLAUSES <", gs,
                       gimple_omp_body (gs));
      dump_omp_clauses (buffer, gimple_omp_for_clauses (gs), spc, flags);
      dump_gimple_fmt (buffer, spc, flags, " >,");
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	dump_gimple_fmt (buffer, spc, flags,
			 "%+%T, %T, %T, %s, %T,%n",
			 gimple_omp_for_index (gs, i),
			 gimple_omp_for_initial (gs, i),
			 gimple_omp_for_final (gs, i),
			 tree_code_name[gimple_omp_for_cond (gs, i)],
			 gimple_omp_for_incr (gs, i));
      dump_gimple_fmt (buffer, spc, flags, "PRE_BODY <%S>%->",
		       gimple_omp_for_pre_body (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp for");
      dump_omp_clauses (buffer, gimple_omp_for_clauses (gs), spc, flags);
      for (i = 0; i < gimple_omp_for_collapse (gs); i++)
	{
	  if (i)
	    spc += 2;
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "for (");
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
	      pp_character (buffer, '<');
	      break;
	    case GT_EXPR:
	      pp_character (buffer, '>');
	      break;
	    case LE_EXPR:
	      pp_string (buffer, "<=");
	      break;
	    case GE_EXPR:
	      pp_string (buffer, ">=");
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
	  pp_character (buffer, ')');
	}

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

/* Dump a GIMPLE_OMP_CONTINUE tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_continue (pretty_printer *buffer, gimple gs, int spc, int flags)
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
      pp_character (buffer, ',');
      pp_space (buffer);
      dump_generic_node (buffer, gimple_omp_continue_control_use (gs),
	  		 spc, flags, false);
      pp_character (buffer, ')');
    }
}

/* Dump a GIMPLE_OMP_SINGLE tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_single (pretty_printer *buffer, gimple gs, int spc, int flags)
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
dump_gimple_omp_sections (pretty_printer *buffer, gimple gs, int spc,
			  int flags)
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
	  pp_character (buffer, '>');
	}
      dump_omp_clauses (buffer, gimple_omp_sections_clauses (gs), spc, flags);
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

/* Dump a GIMPLE_OMP_{MASTER,ORDERED,SECTION} tuple on the pretty_printer
   BUFFER.  */

static void
dump_gimple_omp_block (pretty_printer *buffer, gimple gs, int spc, int flags)
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
	case GIMPLE_OMP_ORDERED:
	  pp_string (buffer, "#pragma omp ordered");
	  break;
	case GIMPLE_OMP_SECTION:
	  pp_string (buffer, "#pragma omp section");
	  break;
	default:
	  gcc_unreachable ();
	}
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

/* Dump a GIMPLE_OMP_CRITICAL tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_critical (pretty_printer *buffer, gimple gs, int spc,
			  int flags)
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
	  pp_character (buffer, ')');
	}
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

/* Dump a GIMPLE_OMP_RETURN tuple on the pretty_printer BUFFER.  */

static void
dump_gimple_omp_return (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <nowait=%d>", gs,
                       (int) gimple_omp_return_nowait_p (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp return");
      if (gimple_omp_return_nowait_p (gs))
	pp_string (buffer, "(nowait)");
    }
}

/* Dump a GIMPLE_ASM tuple on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  */

static void
dump_gimple_asm (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  unsigned int i;

  if (flags & TDF_RAW)
    dump_gimple_fmt (buffer, spc, flags, "%G <%+STRING <%n%s%n>", gs,
                     gimple_asm_string (gs));
  else
    {
      pp_string (buffer, "__asm__");
      if (gimple_asm_volatile_p (gs))
	pp_string (buffer, " __volatile__");
      pp_string (buffer, "(\"");
      pp_string (buffer, gimple_asm_string (gs));
      pp_string (buffer, "\"");
    }

  if (gimple_asm_ninputs (gs)
     || gimple_asm_noutputs (gs) 
     || gimple_asm_nclobbers (gs))
    {
      if (gimple_asm_noutputs (gs))
        {
          if (flags & TDF_RAW)
            {
              newline_and_indent (buffer, spc + 2);
              pp_string (buffer, "OUTPUT: ");
            }
          else
            pp_string (buffer, " : ");
        }

      for (i = 0; i < gimple_asm_noutputs (gs); i++)
        {
          dump_generic_node (buffer, gimple_asm_output_op (gs, i), spc, flags,
                             false);
          if ( i < gimple_asm_noutputs (gs) -1)
            pp_string (buffer, ", ");
        }

      if (gimple_asm_ninputs (gs))
        {
          if (flags & TDF_RAW)
            {
              newline_and_indent (buffer, spc + 2);
              pp_string (buffer, "INPUT: ");
            }
          else
            pp_string (buffer, " : ");
        }

      for (i = 0; i < gimple_asm_ninputs (gs); i++)
        {
          dump_generic_node (buffer, gimple_asm_input_op (gs, i), spc, flags,
                             false);
          if (i < gimple_asm_ninputs (gs) -1)
            pp_string (buffer, " : ");
        }

      if (gimple_asm_nclobbers (gs))
        {
          if (flags & TDF_RAW)
            {
              newline_and_indent (buffer, spc + 2);
              pp_string (buffer, "CLOBBER: ");
            }
          else
            pp_string (buffer, " : ");
        }

      for (i = 0; i < gimple_asm_nclobbers (gs); i++)
        {
          dump_generic_node (buffer, gimple_asm_clobber_op (gs, i), spc, flags,
                             false);
          if ( i < gimple_asm_nclobbers (gs) -1)
            pp_string (buffer, ", ");
        }
    }
  if (flags & TDF_RAW)
    {
      newline_and_indent (buffer, spc);
      pp_character (buffer, '>');
    }
  else
    pp_string (buffer, ");");
}


/* Dump a PHI node PHI.  BUFFER, SPC and FLAGS are as in
   dump_gimple_stmt.  */

static void
dump_gimple_phi (pretty_printer *buffer, gimple phi, int spc, int flags)
{
  size_t i;

  if (flags & TDF_RAW)
      dump_gimple_fmt (buffer, spc, flags, "%G <%T, ", phi,
                       gimple_phi_result (phi));
  else
    {
      dump_generic_node (buffer, gimple_phi_result (phi), spc, flags, false);
      pp_string (buffer, " = PHI <");
    }
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      dump_generic_node (buffer, gimple_phi_arg_def (phi, i), spc, flags,
			 false);
      pp_character (buffer, '(');
      pp_decimal_int (buffer, gimple_phi_arg_edge (phi, i)->src->index);
      pp_character (buffer, ')');
      if (i < gimple_phi_num_args (phi) - 1)
	pp_string (buffer, ", ");
    }
  pp_character (buffer, '>');
}


/* Dump a GIMPLE_OMP_PARALLEL tuple on the pretty_printer BUFFER, SPC spaces
   of indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  */

static void
dump_gimple_omp_parallel (pretty_printer *buffer, gimple gs, int spc,
                          int flags)
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
	  pp_character (buffer, '{');
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '}');
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
   tree-pass.h).  */

static void
dump_gimple_omp_task (pretty_printer *buffer, gimple gs, int spc,
		      int flags)
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
	  pp_character (buffer, '{');
	  pp_newline (buffer);
	  dump_gimple_seq (buffer, body, spc + 4, flags);
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '}');
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
   in tree-pass.h).  */

static void
dump_gimple_omp_atomic_load (pretty_printer *buffer, gimple gs, int spc,
                             int flags)
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
      newline_and_indent (buffer, spc + 2);
      dump_generic_node (buffer, gimple_omp_atomic_load_lhs (gs),
	  		 spc, flags, false);
      pp_space (buffer);
      pp_character (buffer, '=');
      pp_space (buffer);
      pp_character (buffer, '*');
      dump_generic_node (buffer, gimple_omp_atomic_load_rhs (gs),
	  		 spc, flags, false);
    }
}

/* Dump a GIMPLE_OMP_ATOMIC_STORE tuple on the pretty_printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see TDF_*
   in tree-pass.h).  */

static void
dump_gimple_omp_atomic_store (pretty_printer *buffer, gimple gs, int spc,
                             int flags)
{
  if (flags & TDF_RAW)
    {
      dump_gimple_fmt (buffer, spc, flags, "%G <%T>", gs,
                       gimple_omp_atomic_store_val (gs));
    }
  else
    {
      pp_string (buffer, "#pragma omp atomic_store (");
      dump_generic_node (buffer, gimple_omp_atomic_store_val (gs),
	  		 spc, flags, false);
      pp_character (buffer, ')');
    }
}


/* Dump all the memory operands for statement GS.  BUFFER, SPC and
   FLAGS are as in dump_gimple_stmt.  */

static void
dump_gimple_mem_ops (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  tree vdef = gimple_vdef (gs);
  tree vuse = gimple_vuse (gs);

  if (!ssa_operands_active () || !gimple_references_memory_p (gs))
    return;

  if (vdef != NULL_TREE)
    {
      pp_string (buffer, "# ");
      dump_generic_node (buffer, vdef, spc + 2, flags, false);
      pp_string (buffer, " = VDEF <");
      dump_generic_node (buffer, vuse, spc + 2, flags, false);
      pp_character (buffer, '>');
      newline_and_indent (buffer, spc);
    }
  else if (vuse != NULL_TREE)
    {
      pp_string (buffer, "# VUSE <");
      dump_generic_node (buffer, vuse, spc + 2, flags, false);
      pp_character (buffer, '>');
      newline_and_indent (buffer, spc);
    }
}


/* Dump the gimple statement GS on the pretty printer BUFFER, SPC
   spaces of indent.  FLAGS specifies details to show in the dump (see
   TDF_* in tree-pass.h).  */

void
dump_gimple_stmt (pretty_printer *buffer, gimple gs, int spc, int flags)
{
  if (!gs)
    return;

  if (flags & TDF_STMTADDR)
    pp_printf (buffer, "<&%p> ", (void *) gs);

  if ((flags & TDF_LINENO) && gimple_has_location (gs))
    {
      expanded_location xloc = expand_location (gimple_location (gs));
      pp_character (buffer, '[');
      if (xloc.file)
	{
	  pp_string (buffer, xloc.file);
	  pp_string (buffer, " : ");
	}
      pp_decimal_int (buffer, xloc.line);
      pp_string (buffer, ":");
      pp_decimal_int (buffer, xloc.column);
      pp_string (buffer, "] ");
    }

  if (flags & TDF_EH)
    {
      int eh_region = lookup_stmt_eh_region_fn (cfun, gs);
      if (eh_region >= 0)
	pp_printf (buffer, "[EH #%d] ", eh_region);
    }

  if ((flags & (TDF_VOPS|TDF_MEMSYMS))
      && gimple_has_mem_ops (gs))
    dump_gimple_mem_ops (buffer, gs, spc, flags);

  switch (gimple_code (gs))
    {
    case GIMPLE_ASM:
      dump_gimple_asm (buffer, gs, spc, flags);
      break;

    case GIMPLE_ASSIGN:
      dump_gimple_assign (buffer, gs, spc, flags);
      break;

    case GIMPLE_BIND:
      dump_gimple_bind (buffer, gs, spc, flags);
      break;

    case GIMPLE_CALL:
      dump_gimple_call (buffer, gs, spc, flags);
      break;

    case GIMPLE_COND:
      dump_gimple_cond (buffer, gs, spc, flags);
      break;

    case GIMPLE_LABEL:
      dump_gimple_label (buffer, gs, spc, flags);
      break;

    case GIMPLE_GOTO:
      dump_gimple_goto (buffer, gs, spc, flags);
      break;

    case GIMPLE_NOP:
      pp_string (buffer, "GIMPLE_NOP");
      break;

    case GIMPLE_RETURN:
      dump_gimple_return (buffer, gs, spc, flags);
      break;

    case GIMPLE_SWITCH:
      dump_gimple_switch (buffer, gs, spc, flags);
      break;

    case GIMPLE_TRY:
      dump_gimple_try (buffer, gs, spc, flags);
      break;

    case GIMPLE_PHI:
      dump_gimple_phi (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_PARALLEL:
      dump_gimple_omp_parallel (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_TASK:
      dump_gimple_omp_task (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
      dump_gimple_omp_atomic_load (buffer, gs, spc, flags);

      break;

    case GIMPLE_OMP_ATOMIC_STORE:
      dump_gimple_omp_atomic_store (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_FOR:
      dump_gimple_omp_for (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_CONTINUE:
      dump_gimple_omp_continue (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_SINGLE:
      dump_gimple_omp_single (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_RETURN:
      dump_gimple_omp_return (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS:
      dump_gimple_omp_sections (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_SECTIONS_SWITCH:
      pp_string (buffer, "GIMPLE_SECTIONS_SWITCH");
      break;

    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SECTION:
      dump_gimple_omp_block (buffer, gs, spc, flags);
      break;

    case GIMPLE_OMP_CRITICAL:
      dump_gimple_omp_critical (buffer, gs, spc, flags);
      break;

    case GIMPLE_CATCH:
      dump_gimple_catch (buffer, gs, spc, flags);
      break;

    case GIMPLE_EH_FILTER:
      dump_gimple_eh_filter (buffer, gs, spc, flags);
      break;

    case GIMPLE_RESX:
      dump_gimple_resx (buffer, gs, spc, flags);
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

    default:
      GIMPLE_NIY;
    }

  /* If we're building a diagnostic, the formatted text will be
     written into BUFFER's stream by the caller; otherwise, write it
     now.  */
  if (!(flags & TDF_DIAGNOSTIC))
    pp_write_text_to_stream (buffer);
}


/* Dumps header of basic block BB to buffer BUFFER indented by INDENT
   spaces and details described by flags.  */

static void
dump_bb_header (pretty_printer *buffer, basic_block bb, int indent, int flags)
{
  edge e;
  gimple stmt;
  edge_iterator ei;

  if (flags & TDF_BLOCKS)
    {
      INDENT (indent);
      pp_string (buffer, "# BLOCK ");
      pp_decimal_int (buffer, bb->index);
      if (bb->frequency)
	{
          pp_string (buffer, " freq:");
          pp_decimal_int (buffer, bb->frequency);
	}
      if (bb->count)
	{
          pp_string (buffer, " count:");
          pp_widest_integer (buffer, bb->count);
	}

      if (flags & TDF_LINENO)
	{
	  gimple_stmt_iterator gsi;

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    if (get_lineno (gsi_stmt (gsi)) != -1)
	      {
		pp_string (buffer, ", starting at line ");
		pp_decimal_int (buffer, get_lineno (gsi_stmt (gsi)));
		break;
	      }

          if (bb->discriminator)
            {
              pp_string (buffer, ", discriminator ");
	      pp_decimal_int (buffer, bb->discriminator);
            }
	}
      newline_and_indent (buffer, indent);

      pp_string (buffer, "# PRED:");
      pp_write_text_to_stream (buffer);
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (flags & TDF_SLIM)
	  {
	    pp_character (buffer, ' ');
	    if (e->src == ENTRY_BLOCK_PTR)
	      pp_string (buffer, "ENTRY");
	    else
	      pp_decimal_int (buffer, e->src->index);
	  }
	else
	  dump_edge_info (buffer->buffer->stream, e, 0);
      pp_newline (buffer);
    }
  else
    {
      stmt = first_stmt (bb);
      if (!stmt || gimple_code (stmt) != GIMPLE_LABEL)
	{
	  INDENT (indent - 2);
	  pp_string (buffer, "<bb ");
	  pp_decimal_int (buffer, bb->index);
	  pp_string (buffer, ">:");
	  pp_newline (buffer);
	}
    }
  pp_write_text_to_stream (buffer);
  check_bb_profile (bb, buffer->buffer->stream);
}


/* Dumps end of basic block BB to buffer BUFFER indented by INDENT
   spaces.  */

static void
dump_bb_end (pretty_printer *buffer, basic_block bb, int indent, int flags)
{
  edge e;
  edge_iterator ei;

  INDENT (indent);
  pp_string (buffer, "# SUCC:");
  pp_write_text_to_stream (buffer);
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (flags & TDF_SLIM)
      {
	pp_character (buffer, ' ');
	if (e->dest == EXIT_BLOCK_PTR)
	  pp_string (buffer, "EXIT");
	else
	  pp_decimal_int (buffer, e->dest->index);
      }
    else
      dump_edge_info (buffer->buffer->stream, e, 1);
  pp_newline (buffer);
}


/* Dump PHI nodes of basic block BB to BUFFER with details described
   by FLAGS and indented by INDENT spaces.  */

static void
dump_phi_nodes (pretty_printer *buffer, basic_block bb, int indent, int flags)
{
  gimple_stmt_iterator i;

  for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
    {
      gimple phi = gsi_stmt (i);
      if (is_gimple_reg (gimple_phi_result (phi)) || (flags & TDF_VOPS))
        {
          INDENT (indent);
          pp_string (buffer, "# ");
          dump_gimple_phi (buffer, phi, indent, flags);
          pp_newline (buffer);
        }
    }
}


/* Dump jump to basic block BB that is represented implicitly in the cfg
   to BUFFER.  */

static void
pp_cfg_jump (pretty_printer *buffer, basic_block bb)
{
  gimple stmt;

  stmt = first_stmt (bb);

  pp_string (buffer, "goto <bb ");
  pp_decimal_int (buffer, bb->index);
  pp_character (buffer, '>');
  if (stmt && gimple_code (stmt) == GIMPLE_LABEL)
    {
      pp_string (buffer, " (");
      dump_generic_node (buffer, gimple_label_label (stmt), 0, 0, false);
      pp_character (buffer, ')');
      pp_semicolon (buffer);
    }
  else
    pp_semicolon (buffer);
}


/* Dump edges represented implicitly in basic block BB to BUFFER, indented
   by INDENT spaces, with details given by FLAGS.  */

static void
dump_implicit_edges (pretty_printer *buffer, basic_block bb, int indent,
		     int flags)
{
  edge e;
  edge_iterator ei;
  gimple stmt;

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
      pp_cfg_jump (buffer, true_edge->dest);
      newline_and_indent (buffer, indent);
      pp_string (buffer, "else");
      newline_and_indent (buffer, indent + 2);
      pp_cfg_jump (buffer, false_edge->dest);
      pp_newline (buffer);
      return;
    }

  /* If there is a fallthru edge, we may need to add an artificial
     goto to the dump.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_FALLTHRU)
      break;

  if (e && e->dest != bb->next_bb)
    {
      INDENT (indent);

      if ((flags & TDF_LINENO)
	  && e->goto_locus != UNKNOWN_LOCATION
	  )
	{
	  expanded_location goto_xloc;
	  goto_xloc = expand_location (e->goto_locus);
	  pp_character (buffer, '[');
	  if (goto_xloc.file)
	    {
	      pp_string (buffer, goto_xloc.file);
	      pp_string (buffer, " : ");
	    }
	  pp_decimal_int (buffer, goto_xloc.line);
	  pp_string (buffer, " : ");
	  pp_decimal_int (buffer, goto_xloc.column);
	  pp_string (buffer, "] ");
	}

      pp_cfg_jump (buffer, e->dest);
      pp_newline (buffer);
    }
}


/* Dumps basic block BB to buffer BUFFER with details described by FLAGS and
   indented by INDENT spaces.  */

static void
gimple_dump_bb_buff (pretty_printer *buffer, basic_block bb, int indent,
		     int flags)
{
  gimple_stmt_iterator gsi;
  gimple stmt;
  int label_indent = indent - 2;

  if (label_indent < 0)
    label_indent = 0;

  dump_bb_header (buffer, bb, indent, flags);
  dump_phi_nodes (buffer, bb, indent, flags);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      int curr_indent;

      stmt = gsi_stmt (gsi);

      curr_indent = gimple_code (stmt) == GIMPLE_LABEL ? label_indent : indent;

      INDENT (curr_indent);
      dump_gimple_stmt (buffer, stmt, curr_indent, flags);
      pp_newline (buffer);
      dump_histograms_for_stmt (cfun, buffer->buffer->stream, stmt);
    }

  dump_implicit_edges (buffer, bb, indent, flags);

  if (flags & TDF_BLOCKS)
    dump_bb_end (buffer, bb, indent, flags);
}


/* Dumps basic block BB to FILE with details described by FLAGS and
   indented by INDENT spaces.  */

void
gimple_dump_bb (basic_block bb, FILE *file, int indent, int flags)
{
  maybe_init_pretty_print (file);
  gimple_dump_bb_buff (&buffer, bb, indent, flags);
  pp_flush (&buffer);
}
