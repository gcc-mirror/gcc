/* ALGOL 68 parser.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/*
   This is a Mailloux-type parser driver.

   The Algol 68 grammar is a two level (Van Wijngaarden, "VW") grammar
   that incorporates, as syntactical rules, the semantical rules in
   other languages. Examples are correct use of symbols, modes and
   scope.

   This code constitutes an effective "VW Algol 68 parser". A
   pragmatic approach was chosen since in the early days of Algol 68,
   many "ab initio" implementations failed, probably because
   techniques to parse a language like Algol 68 had yet to be
   invented.

   This is a Mailloux-type parser, in the sense that it scans a
   "phrase" for definitions needed for parsing. Algol 68 allows for
   tags to be used before they are defined, which gives freedom in
   top-down programming.

     B. J. Mailloux. On the implementation of Algol 68.
     Thesis, Universiteit van Amsterdam (Mathematisch Centrum) [1968].

   Technically, Mailloux's approach renders the two-level grammar
   LALR.

   First part of the parser is the scanner. The source file is read, is
   tokenised.  The result is a linear list of tokens that is input for the
   parser, that will transform the linear list into a syntax tree.

   This front-end tokenises all symbols before the bottom-up parser is invoked.
   This means that scanning does not use information from the parser.  The
   scanner does of course some rudimentary parsing.

   The scanner supports two stropping regimes: "bold" (or "upper") and
   "quote".  Examples of both:

    bold stropping: BEGIN INT i = 1, j = 1; print (i + j) END

    quote stropping: 'BEGIN' 'INT' I = 1, J = 1; PRINT (I + J) 'END'

   Quote stropping was used frequently in the (excusez-le-mot)
   punch-card age.  Hence, bold stropping is the default. There also
   existed point stropping, but that has not been implemented here.

   Next part of the parser is a recursive-descent type to check
   parenthesis.  Also a first set-up is made of symbol tables, needed
   by the bottom-up parser.  Next part is the bottom-up parser, that
   parses without knowing modes while parsing and reducing. It can
   therefore not exchange "[]" with "()" as was blessed by the Revised
   Report. This is solved by treating CALL and SLICE as equivalent for
   the moment and letting the mode checker sort it out later.

   Parsing progresses in various phases to avoid spurious diagnostics
   from a recovering parser. Every phase "tightens" the grammar more.
   An error in any phase makes the parser quit when that phase ends.
   The parser is forgiving in case of superfluous semicolons.

   These are the parser phases:

   (1) Parenthesis are checked to see whether they match. Then, a top-down
       parser determines the basic-block structure of the program
       so symbol tables can be set up that the bottom-up parser will consult
       as you can define things before they are applied.

   (2) A bottom-up parser resolves the structure of the program.

   (3) After the symbol tables have been finalised, a small rearrangement of the
       tree may be required where JUMPs have no GOTO. This leads to the
       non-standard situation that JUMPs without GOTO can have the syntactic
       position of a PRIMARY, SECONDARY or TERTIARY. The bottom-up parser also
       does not check VICTAL correctness of declarers. This is done separately.

  The parser sets up symbol tables and populates them as far as needed to parse
  the source. After the bottom-up parser terminates succesfully, the symbol tables
  are completed.

   (4) Next, modes are collected and rules for well-formedness and structural
       equivalence are applied. Then the symbol-table is completed now moids are
       all known.

   (5) Next phases are the mode checker and coercion inserter. The syntax tree is
       traversed to determine and check all modes, and to select operators. Then
       the tree is traversed again to insert coercions.

   (6) A static scope checker detects where objects are transported out of scope.
       At run time, a dynamic scope checker will check that what the static scope
       checker cannot see.

   (7) A serial-clause dynamic stack allocation (DSA) phase annotates the
       serial clauses that contain phrases whose elaboration may result in
       dynamic stack adjustments.
*/

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "tree.h"

#include "a68.h"

/* Global state kept by the parser.  */

PARSER_T a68_parser_state;

/* A few forward declarations of functions defined below.  */

static void make_special_mode (MOID_T ** n, int m);
static void tie_label_to_serial (NODE_T *p);
static void tie_label_to_unit (NODE_T *p);

/* Is_ref_refety_flex.  */

bool
a68_is_ref_refety_flex (MOID_T *m)
{
  if (IS_REF_FLEX (m))
    return true;
  else if (IS_REF (m))
    return a68_is_ref_refety_flex (SUB (m));
  else
    return false;
}

/* Count number of operands in operator parameter list.  */

int
a68_count_operands (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, DECLARER))
	return a68_count_operands (NEXT (p));
      else if (IS (p, COMMA_SYMBOL))
	return 1 + a68_count_operands (NEXT (p));
      else
	return a68_count_operands (NEXT (p)) + a68_count_operands (SUB (p));
    }
  else
    return 0;
}

/* Count formal bounds in declarer in tree.  */

int
a68_count_formal_bounds (NODE_T * p)
{
  if (p == NO_NODE)
    return 0;
  else
    {
      if (IS (p, COMMA_SYMBOL))
	return 1;
      else
	return a68_count_formal_bounds (NEXT (p)) + a68_count_formal_bounds (SUB (p));
    }
}

/* Count pictures.  */

void
a68_count_pictures (NODE_T *p, int *k)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, PICTURE))
	(*k)++;
      a68_count_pictures (SUB (p), k);
    }
}

/* Whether token cannot follow semicolon or EXIT.  */

bool
a68_is_semicolon_less (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case BUS_SYMBOL:
    case CLOSE_SYMBOL:
    case END_SYMBOL:
    case SEMI_SYMBOL:
    case EXIT_SYMBOL:
    case THEN_BAR_SYMBOL:
    case ELSE_BAR_SYMBOL:
    case THEN_SYMBOL:
    case ELIF_SYMBOL:
    case ELSE_SYMBOL:
    case FI_SYMBOL:
    case IN_SYMBOL:
    case OUT_SYMBOL:
    case OUSE_SYMBOL:
    case ESAC_SYMBOL:
    case OD_SYMBOL:
    case FED_SYMBOL:
    case POSTLUDE_SYMBOL:
      return true;
    default:
      return false;
    }
}

/* Whether formal bounds.  */

bool
a68_is_formal_bounds (NODE_T *p)
{
  if (p == NO_NODE)
    return true;

  switch (ATTRIBUTE (p))
    {
    case OPEN_SYMBOL:
    case CLOSE_SYMBOL:
    case SUB_SYMBOL:
    case BUS_SYMBOL:
    case COMMA_SYMBOL:
    case COLON_SYMBOL:
    case INT_DENOTATION:
    case IDENTIFIER:
    case OPERATOR:
      return (a68_is_formal_bounds (SUB (p))
	      && a68_is_formal_bounds (NEXT (p)));
    default:
        return false;
    }
}

/* Whether token terminates a unit.  */

bool
a68_is_unit_terminator (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case BUS_SYMBOL:
    case CLOSE_SYMBOL:
    case END_SYMBOL:
    case SEMI_SYMBOL:
    case EXIT_SYMBOL:
    case COMMA_SYMBOL:
    case THEN_BAR_SYMBOL:
    case ELSE_BAR_SYMBOL:
    case THEN_SYMBOL:
    case ELIF_SYMBOL:
    case ELSE_SYMBOL:
    case FI_SYMBOL:
    case IN_SYMBOL:
    case OUT_SYMBOL:
    case OUSE_SYMBOL:
    case ESAC_SYMBOL:
    case FED_SYMBOL:
    case POSTLUDE_SYMBOL:
      return true;
    default:
      return false;
    }
}

/* Whether token is a unit-terminator in a loop clause.  */

bool
a68_is_loop_keyword (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case FOR_SYMBOL:
    case FROM_SYMBOL:
    case BY_SYMBOL:
    case TO_SYMBOL:
    case WHILE_SYMBOL:
    case DO_SYMBOL:
      return true;
    default:
      return false;
    }
}

/* Get good attribute.  */

enum a68_attribute
a68_get_good_attribute (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case UNIT:
    case TERTIARY:
    case SECONDARY:
    case PRIMARY:
      return a68_get_good_attribute (SUB (p));
    default:
      return ATTRIBUTE (p);
    }
}

/* Preferably don't put intelligible diagnostic here.  */

bool
a68_dont_mark_here (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case ALT_DO_SYMBOL:
    case ALT_EQUALS_SYMBOL:
    case ANDF_SYMBOL:
    case ASSERT_SYMBOL:
    case ASSIGN_SYMBOL:
    case ASSIGN_TO_SYMBOL:
    case AT_SYMBOL:
    case BEGIN_SYMBOL:
    case BITS_SYMBOL:
    case BOLD_COMMENT_SYMBOL:
    case BOLD_PRAGMAT_SYMBOL:
    case BOLD_COMMENT_BEGIN_SYMBOL:
    case BOLD_COMMENT_END_SYMBOL:
    case BOOL_SYMBOL:
    case BUS_SYMBOL:
    case BY_SYMBOL:
    case BYTES_SYMBOL:
    case CASE_SYMBOL:
    case CHANNEL_SYMBOL:
    case CHAR_SYMBOL:
    case CLOSE_SYMBOL:
    case COLON_SYMBOL:
    case COMMA_SYMBOL:
    case COMPLEX_SYMBOL:
    case COMPL_SYMBOL:
    case DO_SYMBOL:
    case ELIF_SYMBOL:
    case ELSE_BAR_SYMBOL:
    case ELSE_SYMBOL:
    case EMPTY_SYMBOL:
    case END_SYMBOL:
    case EQUALS_SYMBOL:
    case ESAC_SYMBOL:
    case EXIT_SYMBOL:
    case FALSE_SYMBOL:
    case FILE_SYMBOL:
    case FI_SYMBOL:
    case FLEX_SYMBOL:
    case FOR_SYMBOL:
    case FROM_SYMBOL:
    case GO_SYMBOL:
    case GOTO_SYMBOL:
    case HEAP_SYMBOL:
    case IF_SYMBOL:
    case IN_SYMBOL:
    case INT_SYMBOL:
    case ISNT_SYMBOL:
    case IS_SYMBOL:
    case LOC_SYMBOL:
    case LONG_SYMBOL:
    case MAIN_SYMBOL:
    case MODE_SYMBOL:
    case NIL_SYMBOL:
    case OD_SYMBOL:
    case OF_SYMBOL:
    case OPEN_SYMBOL:
    case OP_SYMBOL:
    case ORF_SYMBOL:
    case OUSE_SYMBOL:
    case OUT_SYMBOL:
    case PAR_SYMBOL:
    case POINT_SYMBOL:
    case PRIO_SYMBOL:
    case PROC_SYMBOL:
    case REAL_SYMBOL:
    case REF_SYMBOL:
    case ROWS_SYMBOL:
    case ROW_SYMBOL:
    case SEMA_SYMBOL:
    case SEMI_SYMBOL:
    case SHORT_SYMBOL:
    case SKIP_SYMBOL:
    case STRING_SYMBOL:
    case STRUCT_SYMBOL:
    case STYLE_I_COMMENT_SYMBOL:
    case STYLE_II_COMMENT_SYMBOL:
    case STYLE_I_PRAGMAT_SYMBOL:
    case SUB_SYMBOL:
    case THEN_BAR_SYMBOL:
    case THEN_SYMBOL:
    case TO_SYMBOL:
    case TRUE_SYMBOL:
    case UNION_SYMBOL:
    case VOID_SYMBOL:
    case WHILE_SYMBOL:
    case SERIAL_CLAUSE:
    case ENQUIRY_CLAUSE:
    case INITIALISER_SERIES:
    case DECLARATION_LIST:
    case DEF_SYMBOL:
    case FED_SYMBOL:
    case POSTLUDE_SYMBOL:
    case ACCESS_SYMBOL:
      return true;
    default:
      return false;
    }
}

/* Renumber nodes in the given subtree P, starting with number N.  */

static void
renumber_nodes (NODE_T *p, int *n)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      NUMBER (p) = (*n)++;
      renumber_nodes (SUB (p), n);
    }
}

/* Parse an ALGOL 68 source file.  */

void
a68_parser (const char *filename)
{
  int renum = 0;

  /* Initialisation.  */
  A68 (top_keyword) = NO_KEYWORD;
  A68 (top_token) = NO_TOKEN;
  A68_PARSER (error_tag) = (TAG_T *) a68_new_tag ();
  TOP_NODE (&A68_JOB) = NO_NODE;
  TOP_MOID (&A68_JOB) = NO_MOID;
  TOP_LINE (&A68_JOB) = NO_LINE;
  STANDENV_MOID (&A68_JOB) = NO_MOID;
  a68_set_up_tables ();
  ERROR_COUNT (&A68_JOB) = WARNING_COUNT (&A68_JOB) = 0;

  /* Tokeniser.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      bool empty_program;
      bool ok = a68_lexical_analyser (filename, &empty_program);

      if (!ok)
	return;

      if (empty_program)
	a68_error (NO_NODE,
		   "particular program or prelude packet not found in source file");

      TREE_LISTING_SAFE (&A68_JOB) = true;
      renum = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &renum);
    }

  /* Final initialisations.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      A68_STANDENV = NO_TABLE;
      a68_init_postulates ();
      A68 (mode_count) = 0;
      make_special_mode (&M_HIP, A68 (mode_count)++);
      make_special_mode (&M_UNDEFINED, A68 (mode_count)++);
      make_special_mode (&M_ERROR, A68 (mode_count)++);
      make_special_mode (&M_VACUUM, A68 (mode_count)++);
      make_special_mode (&M_C_STRING, A68 (mode_count)++);
      make_special_mode (&M_COLLITEM, A68 (mode_count)++);
    }

  /* Handle pragmats.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    a68_handle_pragmats (TOP_NODE (&A68_JOB));

  /* Top-down parser.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_check_parenthesis (TOP_NODE (&A68_JOB));
      if (ERROR_COUNT (&A68_JOB) == 0)
	{
	  if (OPTION_BRACKETS (&A68_JOB))
	    a68_substitute_brackets (TOP_NODE (&A68_JOB));
	  A68 (symbol_table_count) = 0;
	  A68_STANDENV = a68_new_symbol_table (NO_TABLE);
	  LEVEL (A68_STANDENV) = 0;
	  a68_top_down_parser (TOP_NODE (&A68_JOB));
	  //	  printf ("AFTER TOP-DOWN\n");
	  //	  a68_dump_parse_tree (TOP_NODE (&A68_JOB));
	}

      renum = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &renum);
    }

  /* Standard environment builder.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      TABLE (TOP_NODE (&A68_JOB)) = a68_new_symbol_table (A68_STANDENV);
      a68_make_standard_environ ();
      STANDENV_MOID (&A68_JOB) = TOP_MOID (&A68_JOB);
    }

  /* Bottom-up parser.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_preliminary_symbol_table_setup (TOP_NODE (&A68_JOB));
      //      printf ("AFTER PRELIMINARY SYMBOL TABLE SETUP\n");
      //      a68_dump_parse_tree (TOP_NODE (&A68_JOB), true);
      a68_bottom_up_parser (TOP_NODE (&A68_JOB));
      a68_bottom_up_coalesce_pub (TOP_NODE (&A68_JOB));
      renum = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &renum);
    }

  //  printf ("AFTER BOTTOM-UP\n");
  //  a68_dump_parse_tree (TOP_NODE (&A68_JOB), true);

  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_bottom_up_error_check (TOP_NODE (&A68_JOB));
      a68_victal_checker (TOP_NODE (&A68_JOB));
      if (ERROR_COUNT (&A68_JOB) == 0)
	{
	  a68_finalise_symbol_table_setup (TOP_NODE (&A68_JOB), 1);
	  //	  printf ("AFTER FINALISE SYMBOL TABLE SETUP\n");
	  //	  a68_dump_parse_tree (TOP_NODE (&A68_JOB), true, true);
	  NEST (TABLE (TOP_NODE (&A68_JOB))) = A68 (symbol_table_count) = 3;
	  a68_reset_symbol_table_nest_count (TOP_NODE (&A68_JOB));
	  a68_fill_symbol_table_outer (TOP_NODE (&A68_JOB), TABLE (TOP_NODE (&A68_JOB)));
	  a68_set_nest (TOP_NODE (&A68_JOB), NO_NODE);
	  a68_set_proc_level (TOP_NODE (&A68_JOB), 1);
	}
      renum = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &renum);
    }

  /* Mode table builder.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    a68_make_moid_list (&A68_JOB);
  CROSS_REFERENCE_SAFE (&A68_JOB) = true;

  /* Symbol table builder.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    a68_collect_taxes (TOP_NODE (&A68_JOB));

  /* Post parser.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    a68_rearrange_goto_less_jumps (TOP_NODE (&A68_JOB));

  /* Mode checker.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    a68_mode_checker (TOP_NODE (&A68_JOB));

  /* Coercion inserter.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_coercion_inserter (TOP_NODE (&A68_JOB));
      renum = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &renum);
    }

  /* Application checker.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_mark_moids (TOP_NODE (&A68_JOB));
      a68_mark_auxilliary (TOP_NODE (&A68_JOB));
      a68_jumps_from_procs (TOP_NODE (&A68_JOB));
      a68_warn_for_unused_tags (TOP_NODE (&A68_JOB));
    }

  /* Static scope checker.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      tie_label_to_serial (TOP_NODE (&A68_JOB));
      tie_label_to_unit (TOP_NODE (&A68_JOB));
      a68_bind_routine_tags_to_tree (TOP_NODE (&A68_JOB));
      a68_scope_checker (TOP_NODE (&A68_JOB));
    }

  /* Serial dynamic stack allocation checker.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      a68_serial_dsa (TOP_NODE (&A68_JOB));
    }

  /* Finalise syntax tree.  */
  if (ERROR_COUNT (&A68_JOB) == 0)
    {
      int num = 0;
      renumber_nodes (TOP_NODE (&A68_JOB), &num);
      NEST (TABLE (TOP_NODE (&A68_JOB))) = A68 (symbol_table_count) = 3;
      a68_reset_symbol_table_nest_count (TOP_NODE (&A68_JOB));
    }
}

/* New_node_info.  */

NODE_INFO_T *
a68_new_node_info (void)
{
  NODE_INFO_T *z = ggc_cleared_alloc<NODE_INFO_T> ();

  A68 (new_node_infos)++;
  PROCEDURE_LEVEL (z) = 0;
  CHAR_IN_LINE (z) = NO_TEXT;
  SYMBOL (z) = NO_TEXT;
  PRAGMAT (z) = NO_TEXT;
  PRAGMAT_TYPE (z) = 0;
  PRAGMAT_LINE (z) = NO_LINE;
  PRAGMAT_CHAR_IN_LINE (z) = NO_TEXT;
  COMMENT (z) = NO_TEXT;
  COMMENT_TYPE (z) = 0;
  COMMENT_LINE (z) = NO_LINE;
  COMMENT_CHAR_IN_LINE (z) = NO_TEXT;
  LINE (z) = NO_LINE;
  return z;
}

/* New_genie_info.  */

GINFO_T *
a68_new_genie_info (void)
{
  GINFO_T *z = ggc_cleared_alloc<GINFO_T> ();

  A68 (new_genie_infos)++;
  PARTIAL_PROC (z) = NO_MOID;
  PARTIAL_LOCALE (z) = NO_MOID;
  return z;
}

/* Allocate and return a new parse tree node with proper defaults.  */

NODE_T *
a68_new_node (void)
{
  NODE_T *z = ggc_cleared_alloc<NODE_T> ();

  A68 (new_nodes)++;
  TABLE (z) = NO_TABLE;
  INFO (z) = NO_NINFO;
  GINFO (z) = NO_GINFO;
  ATTRIBUTE (z) = STOP;
  ANNOTATION (z) = STOP;
  MOID (z) = NO_MOID;
  NEXT (z) = NO_NODE;
  PREVIOUS (z) = NO_NODE;
  SUB (z) = NO_NODE;
  NEST (z) = NO_NODE;
  NON_LOCAL (z) = NO_TABLE;
  TAX (z) = NO_TAG;
  SEQUENCE (z) = NO_NODE;
  PACK (z) = NO_PACK;
  CDECL (z) = NULL_TREE;
  DYNAMIC_STACK_ALLOCS (z) = false;
  PUBLICIZED (z) = false;
  return z;
}

/* Some_node.  */

NODE_T *
a68_some_node (const char *t)
{
  NODE_T *z = a68_new_node ();
  INFO (z) = a68_new_node_info ();
  GINFO (z) = a68_new_genie_info ();
  NSYMBOL (z) = t;
  return z;
}

/* New_symbol_table.  */

TABLE_T *
a68_new_symbol_table (TABLE_T *p)
{
  TABLE_T *z = ggc_cleared_alloc<TABLE_T> ();

  NUM (z) = A68 (symbol_table_count);
  LEVEL (z) = A68 (symbol_table_count)++;
  NEST (z) = A68 (symbol_table_count);
  ATTRIBUTE (z) = 0;
  INITIALISE_FRAME (z) = true;
  PROC_OPS (z) = true;
  INITIALISE_ANON (z) = true;
  PREVIOUS (z) = p;
  OUTER (z) = NO_TABLE;
  IDENTIFIERS (z) = NO_TAG;
  OPERATORS (z) = NO_TAG;
  MODULES (z) = NO_TAG;
  PRIO (z) = NO_TAG;
  INDICANTS (z) = NO_TAG;
  LABELS (z) = NO_TAG;
  ANONYMOUS (z) = NO_TAG;
  JUMP_TO (z) = NO_NODE;
  SEQUENCE (z) = NO_NODE;
  PUBLIC_RANGE (z) = false;
  return z;
}

/* New_moid.  */

MOID_T *
a68_new_moid (void)
{
  MOID_T *z = ggc_cleared_alloc<MOID_T> ();

  A68 (new_modes)++;
  ATTRIBUTE (z) = 0;
  NUMBER (z) = 0;
  DIM (z) = 0;
  USE (z) = false;
  HAS_ROWS (z) = false;
  PORTABLE (z) = true;
  DERIVATE (z) = false;
  NODE (z) = NO_NODE;
  PACK (z) = NO_PACK;
  SUB (z) = NO_MOID;
  EQUIVALENT_MODE (z) = NO_MOID;
  SLICE (z) = NO_MOID;
  TRIM (z) = NO_MOID;
  DEFLEXED (z) = NO_MOID;
  NAME (z) = NO_MOID;
  MULTIPLE_MODE (z) = NO_MOID;
  NEXT (z) = NO_MOID;
  CTYPE (z) = NULL_TREE;
  ASM_LABEL (z) = NULL;
  return z;
}

/* New_pack.  */

PACK_T *
a68_new_pack (void)
{
  PACK_T *z = ggc_cleared_alloc<PACK_T> ();

  MOID (z) = NO_MOID;
  TEXT (z) = NO_TEXT;
  NODE (z) = NO_NODE;
  NEXT (z) = NO_PACK;
  PREVIOUS (z) = NO_PACK;
  return z;
}

/* New_tag.  */

TAG_T *
a68_new_tag (void)
{
  TAG_T *z = ggc_cleared_alloc<TAG_T> ();

  STATUS (z) = NULL_MASK;
  TAG_TABLE (z) = NO_TABLE;
  MOID (z) = NO_MOID;
  NODE (z) = NO_NODE;
  UNIT (z) = NO_NODE;
  VALUE (z) = NO_TEXT;
  SCOPE (z) = PRIMAL_SCOPE;
  SCOPE_ASSIGNED (z) = false;
  PRIO (z) = 0;
  USE (z) = false;
  IN_PROC (z) = false;
  HEAP (z) = false;
  YOUNGEST_ENVIRON (z) = PRIMAL_SCOPE;
  LOC_ASSIGNED (z) = false;
  NEXT (z) = NO_TAG;
  BODY (z) = NO_TAG;
  PORTABLE (z) = true;
  VARIABLE (z) = false;
  IS_RECURSIVE (z) = false;
  PUBLICIZED (z) = true; /* XXX */
  EXPORTED (z) = false;
  ASCRIBED_ROUTINE_TEXT (z) = false;
  LOWERER (z) = NO_LOWERER;
  TAX_TREE_DECL (z) = NULL_TREE;
  MOIF (z) = NO_MOIF;
  EXTERN_SYMBOL (z) = NO_TEXT;
  NUMBER (z) = ++A68_PARSER (tag_number);
  return z;
}

/* Make special, internal mode.  */

static void
make_special_mode (MOID_T ** n, int m)
{
  (*n) = a68_new_moid ();
  ATTRIBUTE (*n) = 0;
  NUMBER (*n) = m;
  PACK (*n) = NO_PACK;
  SUB (*n) = NO_MOID;
  EQUIVALENT (*n) = NO_MOID;
  DEFLEXED (*n) = NO_MOID;
  NAME (*n) = NO_MOID;
  SLICE (*n) = NO_MOID;
  TRIM (*n) = NO_MOID;
  ROWED (*n) = NO_MOID;
}

/* Whether attributes match in subsequent nodes.  */

bool
a68_whether (NODE_T * p, ...)
{
  va_list vl;
  va_start (vl, p);
  int a;
  while ((a = va_arg (vl, int)) != STOP)
  {
    if (p != NO_NODE && a == WILDCARD)
      FORWARD (p);
    else if (p != NO_NODE && (a == KEYWORD))
      {
	if (a68_find_keyword_from_attribute (A68 (top_keyword), ATTRIBUTE (p)) != NO_KEYWORD)
	  FORWARD (p);
	else
	  {
	    va_end (vl);
	    return false;
	  }
      }
    else if (p != NO_NODE && (a >= 0 ? a == ATTRIBUTE (p) : (-a) != ATTRIBUTE (p)))
      FORWARD (p);
    else
      {
	va_end (vl);
	return false;
      }
  }
  va_end (vl);
  return true;
}

/* Whether one of a series of attributes matches a node.  */

bool
a68_is_one_of (NODE_T *p, ...)
{
  if (p != NO_NODE)
    {
      bool match = false;
      int a;

      va_list vl;
      va_start (vl, p);
      while ((a = va_arg (vl, int)) != STOP)
	match = (match | IS (p, a));
      va_end (vl);
      return match;
    }
  else
    return false;
}


/* Isolate nodes p-q making p a branch to p-q

   From x - p - a - b - c - q - y
   To   x - t - y
            |
            p - a - b - c - q
*/

void
a68_make_sub (NODE_T *p, NODE_T *q, enum a68_attribute t)
{
  NODE_T *z = a68_new_node ();

  gcc_assert (p != NO_NODE && q != NO_NODE);
  *z = *p;

  if (GINFO (p) != NO_GINFO)
    GINFO (z) = a68_new_genie_info ();

  PREVIOUS (z) = NO_NODE;

  if (p == q)
    NEXT (z) = NO_NODE;
  else
    {
      if (NEXT (p) != NO_NODE)
	PREVIOUS (NEXT (p)) = z;
      NEXT (p) = NEXT (q);
      if (NEXT (p) != NO_NODE)
	PREVIOUS (NEXT (p)) = p;
      NEXT (q) = NO_NODE;
    }

  SUB (p) = z;
  ATTRIBUTE (p) = t;
}

/* Find symbol table at level I.  */

static TABLE_T *
find_level (NODE_T *n, int i)
{
  if (n == NO_NODE)
    return NO_TABLE;
  else
    {
      TABLE_T *s = TABLE (n);

      if (s != NO_TABLE && LEVEL (s) == i)
	return s;
      else if ((s = find_level (SUB (n), i)) != NO_TABLE)
	return s;
      else if ((s = find_level (NEXT (n), i)) != NO_TABLE)
	return s;
      else
	return NO_TABLE;
    }
}

/* Whether P is top of lexical level.  */

bool
a68_is_new_lexical_level (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case ALT_DO_PART:
    case BRIEF_ELIF_PART:
    case BRIEF_OUSE_PART:
    case BRIEF_CONFORMITY_OUSE_PART:
    case CHOICE:
    case CLOSED_CLAUSE:
    case CONDITIONAL_CLAUSE:
    case DO_PART:
    case ELIF_PART:
    case ELSE_PART:
    case CASE_CLAUSE:
    case CASE_CHOICE_CLAUSE:
    case CASE_IN_PART:
    case CASE_OUSE_PART:
    case OUT_PART:
    case ROUTINE_TEXT:
    case SPECIFIED_UNIT:
    case THEN_PART:
    case CONFORMITY_CLAUSE:
    case CONFORMITY_CHOICE:
    case CONFORMITY_IN_PART:
    case CONFORMITY_OUSE_PART:
    case WHILE_PART:
    case DEF_PART:
    case POSTLUDE_PART:
    case ACCESS_CLAUSE:
      return true;
    case MODULE_TEXT:
      /* Module texts introduce an additional lexical level encompassing all
	 its parts only if it is endowed with revelations.  */
      if (SUB (p) != NO_NODE && IS (SUB (p), REVELATION_PART))
	return true;
      else
	return false;
      break;
    default:
      return false;
    }
}

/*
 * Couple of utility functions.
 */

/* Safely append to buffer.  */

void
a68_bufcat (char *dst, const char *src, int len)
{
  if (src != NO_TEXT) {
    char *d = dst;
    const char *s = src;
    int n = len;
// Find end of dst and left-adjust; do not go past end
    for (; n-- != 0 && d[0] != '\0'; d++) {
      ;
    }
    int dlen = (int) (d - dst);
    n = len - dlen;
    if (n > 0) {
      while (s[0] != '\0') {
        if (n != 1) {
          (d++)[0] = s[0];
          n--;
        }
        s++;
      }
      d[0] = '\0';
    }
// Better sure than sorry
    dst[len - 1] = '\0';
  }
}

/* Safely copy to buffer.  */

void
a68_bufcpy (char *dst, const char *src, int len)
{
  if (src != NO_TEXT) {
    char *d = dst;
    const char *s = src;
    int n = len;
// Copy as many as fit
    if (n > 0 && --n > 0) {
      do {
        if (((d++)[0] = (s++)[0]) == '\0') {
          break;
        }
      } while (--n > 0);
    }
    if (n == 0 && len > 0) {
// Not enough room in dst, so terminate
      d[0] = '\0';
    }
// Better sure than sorry
    dst[len - 1] = '\0';
  }
}

/* Make a new copy of concatenated strings.  */

char *
a68_new_string (const char *t, ...)
{
  va_list vl;
  va_start (vl, t);
  const char *q = t;
  if (q == NO_TEXT) {
    va_end (vl);
    return NO_TEXT;
  }
  int len = 0;
  while (q != NO_TEXT) {
    len += (int) strlen (q);
    q = va_arg (vl, char *);
  }
  va_end (vl);
  len++;
  char *z = (char *) xmalloc ((size_t) len);
  z[0] = '\0';
  q = t;
  va_start (vl, t);
  while (q != NO_TEXT) {
    a68_bufcat (z, q, len);
    q = va_arg (vl, char *);
  }
  va_end (vl);
  return z;
}

/*  Tie label to the clause it is defined in.  */

static void
tie_label_to_serial (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, SERIAL_CLAUSE))
	{
	  bool valid_follow;

	  if (NEXT (p) == NO_NODE)
	    valid_follow = true;
	  else if (IS (NEXT (p), CLOSE_SYMBOL))
	    valid_follow = true;
	  else if (IS (NEXT (p), END_SYMBOL))
	    valid_follow = true;
	  else if (IS (NEXT (p), OD_SYMBOL))
	    valid_follow = true;
	  else
	    valid_follow = false;

	  if (valid_follow)
	    JUMP_TO (TABLE (SUB (p))) = NO_NODE;
	}

      tie_label_to_serial (SUB (p));
    }
}

/* Tie label to the clause it is defined in.  */

static void
tie_label (NODE_T *p, NODE_T *unit)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEFINING_IDENTIFIER))
	UNIT (TAX (p)) = unit;
      tie_label (SUB (p), unit);
    }
}

/* Tie label to the clause it is defined in.  */

static void
tie_label_to_unit (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, LABELED_UNIT))
	tie_label (SUB_SUB (p), NEXT_SUB (p));
      tie_label_to_unit (SUB (p));
    }
}

/* Table with attribute names.  */

static const char *attribute_names[] =
{
  "STOP",
#define A68_ATTR(ATTR,DESCR) DESCR,
#include "a68-parser-attrs.def"
#undef A68_ATTR
};

/* Get the name of an attribute.  */

const char *
a68_attribute_name (enum a68_attribute attr)
{
  return attribute_names[attr];
}

/* Get the location of node P as a GCC location.  */

location_t
a68_get_node_location (NODE_T *p)
{
  LINE_T *line = LINE (INFO (p));

  if (line == NO_LINE)
    return UNKNOWN_LOCATION;

  unsigned line_number = NUMBER (line);
  unsigned column_number = CHAR_IN_LINE (INFO (p)) - STRING (line) + 1;
  const char *filename = FILENAME (line);

  location_t gcc_location;

  linemap_add (line_table, LC_ENTER, 0, filename, line_number);
  linemap_line_start (line_table, line_number, 0);
  gcc_location = linemap_position_for_column (line_table, column_number);
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);

  return gcc_location;
}

/* Get the location of POS inside LINE as a GCC location.  */

location_t
a68_get_line_location (LINE_T *line, const char *pos)
{
  location_t loc;

  linemap_add (line_table, LC_ENTER, 0, FILENAME (line), NUMBER (line));
  linemap_line_start (line_table, NUMBER (line), 0);
  loc = linemap_position_for_column (line_table, pos - STRING (line) + 1);
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  return loc;
}
