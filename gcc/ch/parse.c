/* Parser for GNU CHILL (CCITT High-Level Language)  -*- C -*-
   Copyright (C) 1992, 1993, 1998, 1999 Free Software Foundation, Inc.

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

/*
 * This is a two-pass parser.  In pass 1, we collect declarations,
 * ignoring actions and most expressions.  We store only the
 * declarations and close, open and re-lex the input file to save
 * main memory.  We anticipate that the compiler will be processing
 * *very* large single programs which are mechanically generated,
 * and so we want to store a minimum of information between passes.
 *
 * yylex detects the end of the main input file and returns the
 * END_PASS_1 token.  We then re-initialize each CHILL compiler 
 * module's global variables and re-process the input file. The 
 * grant file is output.  If the user has requested it, GNU CHILL 
 * exits at this time - its only purpose was to generate the grant
 * file. Optionally, the compiler may exit if errors were detected 
 * in pass 1.
 *
 * As each symbol scope is entered, we install its declarations into
 * the symbol table. Undeclared types and variables are announced
 * now.
 *
 * Then code is generated.
 */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "ch-tree.h"
#include "lex.h"
#include "actions.h"
#include "tasking.h"
#include "parse.h"
#include "toplev.h"

/* Since parsers are distinct for each language, put the 
   language string definition here.  (fnf) */
const char * const language_string = "GNU CHILL";

/* Common code to be done before expanding any action. */
#define INIT_ACTION { \
	if (! ignoring) emit_line_note (input_filename, lineno); }

/* Pop a scope for an ON handler. */
#define POP_USED_ON_CONTEXT pop_handler(1)

/* Pop a scope for an ON handler that wasn't there. */
#define POP_UNUSED_ON_CONTEXT pop_handler(0)

#define PUSH_ACTION push_action()

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

extern struct rtx_def* gen_label_rtx	      PARAMS ((void));
extern void emit_jump                         PARAMS ((struct rtx_def *));
extern struct rtx_def* emit_label             PARAMS ((struct rtx_def *));

/* This is a hell of a lot easier than getting expr.h included in
   by parse.c.  */
extern struct rtx_def *expand_expr  	PARAMS ((tree, struct rtx_def *,
					       enum machine_mode, int));

static int parse_action				PARAMS ((void));
static void ch_parse_init			PARAMS ((void));
static void check_end_label			PARAMS ((tree, tree));
static void end_function       			PARAMS ((void));
static tree build_prefix_clause			PARAMS ((tree));
static enum terminal PEEK_TOKEN			PARAMS ((void));
static int peek_token_				PARAMS ((int));
static void pushback_token			PARAMS ((int, tree));
static void forward_token_			PARAMS ((void));
static void require				PARAMS ((enum terminal));
static int check_token				PARAMS ((enum terminal));
static int expect				PARAMS ((enum terminal, const char *));
static void define__PROCNAME__			PARAMS ((void));

extern int  lineno;
extern char *input_filename;
extern tree generic_signal_type_node;
extern tree signal_code;
extern int all_static_flag;
extern int ignore_case;
     
#if 0
static int  quasi_signal = 0;  /* 1 if processing a quasi signal decl */
#endif

int parsing_newmode;                       /* 0 while parsing SYNMODE; 
					      1 while parsing NEWMODE. */
int expand_exit_needed = 0;

/* Gets incremented if we see errors such that we don't want to run pass 2. */

int serious_errors = 0;

static tree current_fieldlist;

/* We don't care about expressions during pass 1, except while we're
   parsing the RHS of a SYN definition, or while parsing a mode that
   we need.  NOTE:  This also causes mode expressions to be ignored. */
int  ignoring = 1;              /* 1 to ignore expressions */

/* True if we have seen an action not in a (user) function. */
int seen_action = 0;
int build_constructor = 0;

/* The action_nesting_level of the current procedure body. */ 
int proc_action_level = 0;

/* This is the identifier of the label that prefixes the current action,
   or NULL if there was none.  It is cleared at the end of an action,
   or when starting a nested action list, so get it while you can! */
static tree label      = NULL_TREE;        /* for statement labels */

#if 0
static tree current_block;
#endif

int in_pseudo_module = 0;
int pass = 0;                           /* 0 for init_decl_processing,
					   1 for pass 1, 2 for pass 2 */

/* re-initialize global variables for pass 2 */
static void
ch_parse_init ()
{
  expand_exit_needed = 0;
  label = NULL_TREE;             /* for statement labels */
  current_module = NULL;
  in_pseudo_module = 0;
}

static void
check_end_label (start, end)
     tree start, end;
{
  if (end != NULL_TREE)
    {
      if (start == NULL_TREE && pass == 1)
	error ("there was no start label to match the end label '%s'",
	       IDENTIFIER_POINTER(end));
      else if (start != end && pass == 1)
	error ("start label '%s' does not match end label '%s'",
	       IDENTIFIER_POINTER(start),
	       IDENTIFIER_POINTER(end));
    }
}


/*
 * given a tree which is an id, a type or a decl, 
 * return the associated type, or issue an error and
 * return error_mark_node.
 */
tree
get_type_of (id_or_decl)
     tree id_or_decl;
{
  tree type = id_or_decl;

  if (id_or_decl == NULL_TREE
      || TREE_CODE (id_or_decl) == ERROR_MARK)
    return error_mark_node;

  if (pass == 1 || ignoring == 1)
    return id_or_decl;

  if (TREE_CODE (type) == IDENTIFIER_NODE)
    {
      type = lookup_name (id_or_decl);
      if (type == NULL_TREE)
	{
	  error ("`%s' not declared", IDENTIFIER_POINTER (id_or_decl));
	  type = error_mark_node;
	}
    }
  if (TREE_CODE (type) == TYPE_DECL)
    type = TREE_TYPE (type);
  return type;           /* was a type all along */
}


static void
end_function ()
{
  if (CH_DECL_PROCESS (current_function_decl))
    { 
      /* finishing a process */
      if (! ignoring)
	{
	  tree result = 
	    build_chill_function_call
	      (lookup_name (get_identifier ("__stop_process")),
	       NULL_TREE);
	  expand_expr_stmt (result);
	  emit_line_note (input_filename, lineno);
	}
    }
  else
    {
      /* finishing a procedure.. */
      if (! ignoring)
	{
	  if (result_never_set
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (current_function_decl)))
	      != VOID_TYPE)
	    warning ("No RETURN or RESULT in procedure");
	  chill_expand_return (NULL_TREE, 1);
	}
    }
  finish_chill_function ();
  pop_chill_function_context (); 
}

static tree
build_prefix_clause (id)
     tree id;
{
  if (!id)
    {
      if (current_module && current_module->name)
	{ const char *module_name = IDENTIFIER_POINTER (current_module->name);
	  if (module_name[0] && module_name[0] != '_')
	    return current_module->name;
	}
      error ("PREFIXED clause with no prelix in unlabeled module");
    }
  return id;
}

void
possibly_define_exit_label (label)
     tree label;
{
  if (label)
    define_label (input_filename, lineno, munge_exit_label (label));
}

#define MAX_LOOK_AHEAD 2
static enum terminal terminal_buffer[MAX_LOOK_AHEAD+1];
YYSTYPE yylval;
static YYSTYPE val_buffer[MAX_LOOK_AHEAD+1];

/*enum terminal current_token, lookahead_token;*/

#define TOKEN_NOT_READ dummy_last_terminal

#ifdef __GNUC__
__inline__
#endif
static enum terminal
PEEK_TOKEN()
{
  if (terminal_buffer[0] == TOKEN_NOT_READ)
    {
      terminal_buffer[0] = yylex();
      val_buffer[0] = yylval;
    }
  return terminal_buffer[0];
}
#define PEEK_TREE() val_buffer[0].ttype
#define PEEK_TOKEN1() peek_token_(1)
#define PEEK_TOKEN2() peek_token_(2)
static int
peek_token_ (i)
     int i;
{
  if (i > MAX_LOOK_AHEAD)
    fatal ("internal error - too much lookahead");
  if (terminal_buffer[i] == TOKEN_NOT_READ)
    {
      terminal_buffer[i] = yylex();
      val_buffer[i] = yylval;
    }
  return terminal_buffer[i];
}

static void
pushback_token (code, node)
     int code;
     tree node;
{
  int i;
  if (terminal_buffer[MAX_LOOK_AHEAD] != TOKEN_NOT_READ)
    fatal ("internal error - cannot pushback token");
  for (i = MAX_LOOK_AHEAD; i > 0; i--)
    { 
      terminal_buffer[i] = terminal_buffer[i - 1]; 
      val_buffer[i] = val_buffer[i - 1];
  }
  terminal_buffer[0] = code;
  val_buffer[0].ttype = node;
}

static void
forward_token_()
{
  int i;
  for (i = 0; i < MAX_LOOK_AHEAD; i++)
    {
      terminal_buffer[i] = terminal_buffer[i+1];
      val_buffer[i] = val_buffer[i+1];
    }
  terminal_buffer[MAX_LOOK_AHEAD] = TOKEN_NOT_READ;
}
#define FORWARD_TOKEN() forward_token_()

/* Skip the next token.
   if it isn't TOKEN, the parser is broken. */

static void
require(token)
     enum terminal token;
{
  if (PEEK_TOKEN() != token)
    fatal ("internal parser error - expected token %d", (int)token);
  FORWARD_TOKEN();
}

static int
check_token (token)
     enum terminal token;
{
  if (PEEK_TOKEN() != token)
    return 0;
  FORWARD_TOKEN ();
  return 1;
}

/* return 0 if expected token was not found,
   else return 1.
*/
static int
expect(token, message)
     enum terminal token;
     const char *message;
{
  if (PEEK_TOKEN() != token)
    {
      if (pass == 1)
	error("%s", message ? message : "syntax error");
      return 0;
    }
  else
    FORWARD_TOKEN();
  return 1;
}

/* define a SYNONYM __PROCNAME__ (__procname__) which holds
   the name of the current procedure.
   This should be quit the same as __FUNCTION__ in C */
static void
define__PROCNAME__ ()
{
  const char *fname;
  tree string;
  tree procname;

  if (current_function_decl == NULL_TREE)
    fname = "toplevel";
  else
    fname = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));

  string = build_chill_string (strlen (fname), fname);
  procname = get_identifier (ignore_case ? "__procname__" : "__PROCNAME__");
  push_syndecl (procname, NULL_TREE, string);
}

/* Forward declarations. */
static tree parse_expression PARAMS ((void));
static tree parse_primval PARAMS ((void));
static tree parse_mode PARAMS ((void));
static tree parse_opt_mode PARAMS ((void));
static tree parse_untyped_expr PARAMS ((void));
static tree parse_opt_untyped_expr PARAMS ((void));
static int parse_definition PARAMS ((int));
static void parse_opt_actions PARAMS ((void));
static void parse_body PARAMS ((void));
static tree parse_if_expression_body PARAMS ((void));
static tree parse_opt_handler PARAMS ((void));
static tree parse_opt_name_string PARAMS ((int));
static tree parse_simple_name_string PARAMS ((void));
static tree parse_name_string PARAMS ((void));
static tree parse_defining_occurrence PARAMS ((void));
static tree parse_name PARAMS ((void));
static tree parse_optlabel PARAMS ((void));
static void parse_opt_end_label_semi_colon PARAMS ((tree));
static void parse_modulion PARAMS ((tree));
static void parse_spec_module PARAMS ((tree));
static void parse_semi_colon PARAMS ((void));
static tree parse_defining_occurrence_list PARAMS ((void));
static void parse_mode_definition PARAMS ((int));
static void parse_mode_definition_statement PARAMS ((int));
static void parse_synonym_definition PARAMS ((void));
static void parse_synonym_definition_statement PARAMS ((void));
static tree parse_on_exception_list PARAMS ((void));
static void parse_on_alternatives PARAMS ((void));
static void parse_loc_declaration PARAMS ((int));
static void parse_declaration_statement PARAMS ((int));
static tree parse_optforbid PARAMS ((void));
static tree parse_postfix PARAMS ((enum terminal));
static tree parse_postfix_list PARAMS ((enum terminal));
static void parse_rename_clauses PARAMS ((enum terminal));
static tree parse_opt_prefix_clause PARAMS ((void));
static void parse_grant_statement PARAMS ((void));
static void parse_seize_statement PARAMS ((void));
static tree parse_param_name_list PARAMS ((void));
static tree parse_param_attr PARAMS ((void));
static tree parse_formpar PARAMS ((void));
static tree parse_formparlist PARAMS ((void));
static tree parse_opt_result_spec PARAMS ((void));
static tree parse_opt_except PARAMS ((void));
static tree parse_opt_recursive PARAMS ((void));
static tree parse_procedureattr PARAMS ((void));
static void parse_proc_body PARAMS ((tree, tree));
static void parse_procedure_definition PARAMS ((int));
static tree parse_processpar PARAMS ((void));
static tree parse_processparlist PARAMS ((void));
static void parse_process_definition PARAMS ((int));
static void parse_signal_definition PARAMS ((void));
static void parse_signal_definition_statement PARAMS ((void));
static void parse_then_clause PARAMS ((void));
static void parse_opt_else_clause PARAMS ((void));
static tree parse_expr_list PARAMS ((void));
static tree parse_range_list_clause PARAMS ((void));
static void pushback_paren_expr PARAMS ((tree));
static tree parse_case_label PARAMS ((void));
static tree parse_case_label_list PARAMS ((tree, int));
static tree parse_case_label_specification PARAMS ((tree));
static void parse_single_dimension_case_action PARAMS ((tree));
static void parse_multi_dimension_case_action PARAMS ((tree));
static void parse_case_action PARAMS ((tree));
static tree parse_asm_operands PARAMS ((void));
static tree parse_asm_clobbers PARAMS ((void));
static void ch_expand_asm_operands PARAMS ((tree, tree, tree, tree, int, char *, int));
static void parse_asm_action PARAMS ((void));
static void parse_begin_end_block PARAMS ((tree));
static void parse_if_action PARAMS ((tree));
static void parse_iteration PARAMS ((void));
static tree parse_delay_case_event_list PARAMS ((void));
static void parse_delay_case_action PARAMS ((tree));
static void parse_do_action PARAMS ((tree));
static tree parse_receive_spec PARAMS ((void));
static void parse_receive_case_action PARAMS ((tree));
static void parse_send_action PARAMS ((void));
static void parse_start_action PARAMS ((void));
static tree parse_call PARAMS ((tree));
static tree parse_tuple_fieldname_list PARAMS ((void));
static tree parse_tuple_element PARAMS ((void));
static tree parse_opt_element_list PARAMS ((void));
static tree parse_tuple PARAMS ((tree));
static tree parse_operand6 PARAMS ((void));
static tree parse_operand5 PARAMS ((void));
static tree parse_operand4 PARAMS ((void));
static tree parse_operand3 PARAMS ((void));
static tree parse_operand2 PARAMS ((void));
static tree parse_operand1 PARAMS ((void));
static tree parse_operand0 PARAMS ((void));
static tree parse_case_expression PARAMS ((void));
static tree parse_then_alternative PARAMS ((void));
static tree parse_else_alternative PARAMS ((void));
static tree parse_if_expression PARAMS ((void));
static tree parse_index_mode PARAMS ((void));
static tree parse_set_mode PARAMS ((void));
static tree parse_pos PARAMS ((void));
static tree parse_step PARAMS ((void));
static tree parse_opt_layout PARAMS ((int));
static tree parse_field_name_list PARAMS ((void));
static tree parse_fixed_field PARAMS ((void));
static tree parse_variant_field_list PARAMS ((void));
static tree parse_variant_alternative PARAMS ((void));
static tree parse_field PARAMS ((void));
static tree parse_structure_mode PARAMS ((void));
static tree parse_opt_queue_size PARAMS ((void));
static tree parse_procedure_mode PARAMS ((void));
static void parse_program PARAMS ((void));
static void parse_pass_1_2 PARAMS ((void));

static tree
parse_opt_name_string (allow_all)
     int allow_all; /* 1 if ALL is allowed as a postfix */
{
  enum terminal token = PEEK_TOKEN();
  tree name;
  if (token != NAME)
    {
      if (token == ALL && allow_all)
	{
	  FORWARD_TOKEN ();
	  return ALL_POSTFIX;
	}
      return NULL_TREE;
    }
  name = PEEK_TREE();
  for (;;)
    {
      FORWARD_TOKEN ();
      token = PEEK_TOKEN();
      if (token != '!')
	return name;
      FORWARD_TOKEN();
      token = PEEK_TOKEN();
      if (token == ALL && allow_all)
	return get_identifier3(IDENTIFIER_POINTER (name), "!", "*");
      if (token != NAME)
	{
	  if (pass == 1)
	    error ("'%s!' is not followed by an identifier",
		   IDENTIFIER_POINTER (name));
	  return name;
	}
      name = get_identifier3(IDENTIFIER_POINTER(name),
			     "!", IDENTIFIER_POINTER(PEEK_TREE()));
    }
}

static tree
parse_simple_name_string ()
{
  enum terminal token = PEEK_TOKEN();
  tree name;
  if (token != NAME)
    {
      error ("expected a name here");
      return error_mark_node;
    }
  name = PEEK_TREE ();
  FORWARD_TOKEN ();
  return name;
}

static tree
parse_name_string ()
{
  tree name = parse_opt_name_string (0);
  if (name)
    return name;
  if (pass == 1)
    error ("expected a name string here");
  return error_mark_node;
}

static tree
parse_defining_occurrence ()
{
  if (PEEK_TOKEN () == NAME)
    {
      tree id = PEEK_TREE();
      FORWARD_TOKEN ();
      return id;
    }
  return NULL;
}

/* Matches: <name_string>
   Returns if pass 1: the identifier.
   Returns if pass 2: a decl or value for identifier. */

static tree
parse_name ()
{
  tree name = parse_name_string ();
  if (pass == 1 || ignoring)
    return name;
  else
    {
      tree decl = lookup_name (name);
      if (decl == NULL_TREE)
	{
	  error ("`%s' undeclared", IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}
      else if (TREE_CODE (TREE_TYPE (decl)) == ERROR_MARK)
	return error_mark_node;
      else if (TREE_CODE (decl) == CONST_DECL)
	return DECL_INITIAL (decl);
      else if (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE)
	return convert_from_reference (decl);
      else
	return decl;
    } 
}

static tree
parse_optlabel()
{
  tree label = parse_defining_occurrence();
  if (label != NULL)
    expect(COLON, "expected a ':' here");
  return label;
}

static void
parse_semi_colon ()
{
  enum terminal token = PEEK_TOKEN ();
  if (token == SC)
    FORWARD_TOKEN ();
  else if (pass == 1)
    (token == END ? pedwarn : error) ("expected ';' here");
  label = NULL_TREE;
}

static void
parse_opt_end_label_semi_colon (start_label)
     tree start_label;
{
  if (PEEK_TOKEN() == NAME)
    {
      tree end_label = parse_name_string ();
      check_end_label (start_label, end_label);
    }
  parse_semi_colon ();
}

static void
parse_modulion (label)
     tree label;
{
  tree module_name;

  label = set_module_name (label);
  module_name = push_module (label, 0);
  FORWARD_TOKEN();

  push_action ();
  parse_body();
  expect(END, "expected END here");
  parse_opt_handler ();
  parse_opt_end_label_semi_colon (label);
  find_granted_decls ();
  pop_module ();
}

static void
parse_spec_module (label)
     tree label;
{
  int save_ignoring = ignoring;

  push_module (set_module_name (label), 1);
  ignoring = pass == 2;
  FORWARD_TOKEN(); /* SKIP SPEC */
  expect (MODULE, "expected 'MODULE' here");

  while (parse_definition (1)) { }
  if (parse_action ())
    error ("action not allowed in SPEC MODULE");
  expect(END, "expected END here");
  parse_opt_end_label_semi_colon (label);
  find_granted_decls ();
  pop_module ();
  ignoring = save_ignoring;
}

/* Matches:  <name_string> ( "," <name_string> )*
   Returns either a single IDENTIFIER_NODE,
   or a chain (TREE_LIST) of IDENTIFIER_NODES.
   (Since a single identifier is the common case, we avoid wasting space
   (twice, once for each pass) with extra TREE_LIST nodes in that case.)
   (Will not return NULL_TREE even if ignoring is true.) */

static tree
parse_defining_occurrence_list ()
{
  tree chain = NULL_TREE;
  tree name = parse_defining_occurrence ();
  if (name == NULL_TREE)
    {
      error("missing defining occurrence");
      return NULL_TREE;
    }
  if (! check_token (COMMA))
    return name;
  chain = build_tree_list (NULL_TREE, name);
  for (;;)
    {
      name = parse_defining_occurrence ();
      if (name == NULL)
	{
	  error ("bad defining occurrence following ','");
	  break;
	}
      chain = tree_cons (NULL_TREE, name, chain);
      if (! check_token (COMMA))
	break;
    }
  return nreverse (chain);
}

static void
parse_mode_definition (is_newmode)
     int is_newmode;
{
  tree mode, names;
  int save_ignoring = ignoring;
  ignoring = pass == 2;
  names = parse_defining_occurrence_list ();
  expect (EQL, "missing '=' in mode definition");
  mode = parse_mode ();
  if (names == NULL_TREE || TREE_CODE (names) == TREE_LIST)
    {
      for ( ; names != NULL_TREE; names = TREE_CHAIN (names))
	push_modedef (names, mode, is_newmode);
    }
  else
    push_modedef (names, mode, is_newmode);
  ignoring = save_ignoring;
}

static void
parse_mode_definition_statement (is_newmode)
     int is_newmode;
{
  FORWARD_TOKEN (); /* skip SYNMODE or NEWMODE */
  parse_mode_definition (is_newmode);
  while (PEEK_TOKEN () == COMMA)
    {
      FORWARD_TOKEN ();
      parse_mode_definition (is_newmode);
    }
  parse_semi_colon ();
}

static void
parse_synonym_definition ()
{ tree expr = NULL_TREE;
  tree names = parse_defining_occurrence_list ();
  tree mode = parse_opt_mode ();
  if (! expect (EQL, "missing '=' in synonym definition"))
    mode = error_mark_node;
  else
    {
      if (mode)
	expr = parse_untyped_expr ();
      else
	expr = parse_expression ();
    }
  if (names == NULL_TREE || TREE_CODE (names) == TREE_LIST)
    {
      for ( ; names != NULL_TREE; names = TREE_CHAIN (names))
	push_syndecl (names, mode, expr);
    }
  else
    push_syndecl (names, mode, expr);
}

static void
parse_synonym_definition_statement()
{
  int save_ignoring= ignoring;
  ignoring = pass == 2;
  require (SYN);
  parse_synonym_definition ();
  while (PEEK_TOKEN () == COMMA)
    {
      FORWARD_TOKEN ();
      parse_synonym_definition ();
    }
  ignoring = save_ignoring;
  parse_semi_colon ();
}

/* Attempts to match: "(" <exception list> ")" ":".
   Return NULL_TREE on failure, and non-NULL on success.
   On success, if pass 1, return a TREE_LIST of IDENTIFIER_NODEs. */

static tree
parse_on_exception_list ()
{
  tree name;
  tree list = NULL_TREE;
  int tok1 = PEEK_TOKEN ();
  int tok2 = PEEK_TOKEN1 ();

  /* This requires a lot of look-ahead, because we cannot
     easily a priori distinguish an exception-list from an expression. */
  if (tok1 != LPRN || tok2 != NAME)
    {
      if (tok1 == NAME && tok2 == COLON && pass == 1)
	error ("missing '(' in exception list");
      return 0;
    }
  require (LPRN);
  name = parse_name_string ();
  if (PEEK_TOKEN () == RPRN && PEEK_TOKEN1 () == COLON)
    {
      /* Matched: '(' <name_string> ')' ':' */
      FORWARD_TOKEN (); FORWARD_TOKEN ();
      return pass == 1 ? build_tree_list (NULL_TREE, name) : name;
    }
  if (PEEK_TOKEN() == COMMA)
    {
      if (pass == 1)
	list = build_tree_list (NULL_TREE, name);
      while (check_token (COMMA))
	{
	  tree old_names = list;
	  name = parse_name_string ();
	  if (pass == 1)
	    {
	      for ( ; old_names != NULL_TREE; old_names = TREE_CHAIN (old_names))
		{
		  if (TREE_VALUE (old_names) == name)
		    {
		      error ("ON exception names must be unique");
		      goto continue_parsing;
		    }
		}
	      list = tree_cons (NULL_TREE, name, list);
	    continue_parsing:
	      ;
	    }
	}
      if (! check_token (RPRN) || ! check_token(COLON))
	error ("syntax error in exception list");
      return pass == 1 ? nreverse (list) : name;
    }
  /* Matched: '(' name_string
     but it doesn't match the syntax of an exception list.
     It could be the beginning of an expression, so back up. */
  pushback_token (NAME, name);
  pushback_token (LPRN, 0);
  return NULL_TREE;
}

static void
parse_on_alternatives ()
{
  for (;;)
    {
      tree except_list = parse_on_exception_list ();
      if (except_list != NULL)
	chill_handle_on_labels (except_list);
      else if (parse_action ())
	expand_exit_needed = 1;
      else
	break;
    }
}

static tree
parse_opt_handler ()
{
  if (! check_token (ON))
    {
      POP_UNUSED_ON_CONTEXT;
      return NULL_TREE;
    }
  if (check_token (END))
    {
      pedwarn ("empty ON-condition"); 
      POP_UNUSED_ON_CONTEXT;
      return NULL_TREE;
    } 
  if (! ignoring)
    {
      chill_start_on ();
      expand_exit_needed = 0;
    }
  if (PEEK_TOKEN () != ELSE)
    {
      parse_on_alternatives ();
      if (! ignoring && expand_exit_needed)
	expand_exit_something (); 
    }
  if (check_token (ELSE))
    {
      chill_start_default_handler ();
      label = NULL_TREE;
      parse_opt_actions ();
      if (! ignoring)
	{
	  emit_line_note (input_filename, lineno); 
	  expand_exit_something (); 
	} 
    }
  expect (END, "missing 'END' after");
  if (! ignoring)
    chill_finish_on ();
  POP_USED_ON_CONTEXT;
  return integer_zero_node; 
}

static void
parse_loc_declaration (in_spec_module)
     int in_spec_module;
{
  tree names = parse_defining_occurrence_list ();
  int save_ignoring = ignoring;
  int is_static, lifetime_bound;
  tree mode, init_value = NULL_TREE;
  int loc_decl = 0;

  ignoring = pass == 2;
  mode = parse_mode ();
  ignoring = save_ignoring;
  is_static = check_token (STATIC);
  if (check_token (BASED))
    {
      expect(LPRN, "BASED must be followed by (NAME)");
      do_based_decls (names, mode, parse_name_string ());
      expect(RPRN, "BASED must be followed by (NAME)");
      return;
    }
  if (check_token (LOC))
    {
      /* loc-identity declaration */
      if (pass == 1)
	mode = build_chill_reference_type (mode);
      loc_decl = 1;
    }
  lifetime_bound = check_token (INIT);
  if (lifetime_bound && loc_decl)
    {
      if (pass == 1)
	error ("INIT not allowed at loc-identity declaration");
      lifetime_bound = 0;
    }
  if (PEEK_TOKEN () == ASGN || PEEK_TOKEN() == EQL)
    {
      save_ignoring = ignoring;
      ignoring = pass == 1;
      if (PEEK_TOKEN() == EQL)
	{
	  if (pass == 1)
	    error ("'=' used where ':=' is required");
	}
      FORWARD_TOKEN();
      if (! lifetime_bound)
	push_handler ();
      init_value = parse_untyped_expr ();
      if (in_spec_module)
	{
	  error ("initialization is not allowed in spec module");
	  init_value = NULL_TREE;
	}
      if (! lifetime_bound)
	parse_opt_handler ();
      ignoring = save_ignoring;
    }
  if (init_value == NULL_TREE && loc_decl && pass == 1)
    error ("loc-identity declaration without initialisation");
  do_decls (names, mode,
	    is_static || global_bindings_p ()
	    /* the variable becomes STATIC if all_static_flag is set and
	       current functions doesn't have the RECURSIVE attribute */
	    || (all_static_flag && !CH_DECL_RECURSIVE (current_function_decl)),
	    lifetime_bound, init_value, in_spec_module);

  /* Free any temporaries we made while initializing the decl.  */
  free_temp_slots ();
}

static void
parse_declaration_statement (in_spec_module)
     int in_spec_module;
{
  int save_ignoring = ignoring;
  ignoring = pass == 2;
  require (DCL);
  parse_loc_declaration (in_spec_module);
  while (PEEK_TOKEN () == COMMA)
    {
      FORWARD_TOKEN ();
      parse_loc_declaration (in_spec_module);
    }
  ignoring = save_ignoring;
  parse_semi_colon ();
}

static tree
parse_optforbid ()
{
  if (check_token (FORBID) == 0)
    return NULL_TREE;
  if (check_token (ALL))
    return ignoring ? NULL_TREE : build_int_2 (-1, -1);
#if 0
  if (check_token (LPRN))
    {
      tree list = parse_forbidlist ();
      expect (RPRN, "missing ')' after FORBID list");
      return list;
    }
#endif
  error ("bad syntax following FORBID");
  return NULL_TREE;
}

/* Matches: <grant postfix> or <seize postfix>
   Returns: A (singleton) TREE_LIST. */

static tree
parse_postfix (grant_or_seize)
     enum terminal grant_or_seize;
{
  tree name = parse_opt_name_string (1);
  tree forbid = NULL_TREE;
  if (name == NULL_TREE)
    {
      error ("expected a postfix name here");
      name = error_mark_node;
    }
  if (grant_or_seize == GRANT)
    forbid = parse_optforbid ();
  return build_tree_list (forbid, name);
}

static tree
parse_postfix_list (grant_or_seize)
     enum terminal grant_or_seize;
{
  tree list = parse_postfix (grant_or_seize);
  while (check_token (COMMA))
    list = chainon (list, parse_postfix (grant_or_seize));
  return list;
}

static void
parse_rename_clauses (grant_or_seize)
     enum terminal grant_or_seize;
{
  for (;;)
    {
      tree rename_old_prefix, rename_new_prefix, postfix;
      require (LPRN);
      rename_old_prefix = parse_opt_name_string (0);
      expect (ARROW, "missing '->' in rename clause");
      rename_new_prefix = parse_opt_name_string (0);
      expect (RPRN,  "missing ')' in rename clause");
      expect ('!',  "missing '!' in rename clause");
      postfix = parse_postfix (grant_or_seize);

      if (grant_or_seize == GRANT)
	chill_grant (rename_old_prefix, rename_new_prefix,
		     TREE_VALUE (postfix), TREE_PURPOSE (postfix));
      else
	chill_seize (rename_old_prefix, rename_new_prefix,
		     TREE_VALUE (postfix));

      if (PEEK_TOKEN () != COMMA)
	break;
      FORWARD_TOKEN ();
      if (PEEK_TOKEN () != LPRN)
	{
	  error ("expected another rename clause");
	  break;
	}
    }
}

static tree
parse_opt_prefix_clause ()
{
  if (check_token (PREFIXED) == 0)
    return NULL_TREE;
  return build_prefix_clause (parse_opt_name_string (0));
}

static void
parse_grant_statement ()
{
  require (GRANT);
  if (PEEK_TOKEN () == LPRN)
    parse_rename_clauses (GRANT);
  else
    {
      tree window = parse_postfix_list (GRANT);
      tree new_prefix = parse_opt_prefix_clause ();
      tree t;
      for (t = window; t; t = TREE_CHAIN (t))
	chill_grant (NULL_TREE, new_prefix, TREE_VALUE (t), TREE_PURPOSE (t));
    }
}

static void
parse_seize_statement ()
{
  require (SEIZE);
  if (PEEK_TOKEN () == LPRN)
    parse_rename_clauses (SEIZE);
  else
    {
      tree seize_window = parse_postfix_list (SEIZE);
      tree old_prefix = parse_opt_prefix_clause ();
      tree t;
      for (t = seize_window; t; t = TREE_CHAIN (t))
	chill_seize (old_prefix, NULL_TREE, TREE_VALUE (t));
    }
}

/* In pass 1, this returns a TREE_LIST, one node for each parameter.
   In pass 2, we get a list of PARM_DECLs chained together.
   In either case, the list is in reverse order. */

static tree
parse_param_name_list ()
{
  tree list = NULL_TREE;
  do
    {
      tree new_link;
      tree name = parse_defining_occurrence ();
      if (name == NULL_TREE)
	{
	  error ("syntax error in parameter name list");
	  return list;
	}
      if (pass == 1)
	new_link = build_tree_list (NULL_TREE, name);
      /* else if (current_module->is_spec_module) ; nothing */
      else  /* pass == 2 */
	{
	  new_link = make_node (PARM_DECL);
	  DECL_NAME (new_link) = name;
	  DECL_ASSEMBLER_NAME (new_link) = name;
	}

      TREE_CHAIN (new_link) = list;
      list = new_link;
    } while (check_token (COMMA));
  return list;
}

static tree
parse_param_attr ()
{
  tree attr;
  switch (PEEK_TOKEN ())
    {
    case PARAMATTR:          /* INOUT is returned here */
      attr = PEEK_TREE ();
      FORWARD_TOKEN ();
      return attr;
    case IN:
      FORWARD_TOKEN ();
      return ridpointers[(int) RID_IN];
    case LOC:
      FORWARD_TOKEN ();
      return ridpointers[(int) RID_LOC];
#if 0
    case DYNAMIC:
      FORWARD_TOKEN ();
      return ridpointers[(int) RID_DYNAMIC];
#endif
    default:
      return NULL_TREE;
    }
}

/* We wrap CHILL array parameters in a STRUCT.  The original parameter
   name is unpacked from the struct at get_identifier time */

/* In pass 1, returns list of types; in pass 2: chain of PARM_DECLs. */
   
static tree
parse_formpar ()
{
  tree names = parse_param_name_list ();
  tree mode = parse_mode ();
  tree paramattr = parse_param_attr ();
  return chill_munge_params (nreverse (names), mode, paramattr);
}

/*
 * Note: build_process_header depends upon the *exact*
 * representation of STRUCT fields and of formal parameter
 * lists.  If either is changed, build_process_header will
 * also need change.  Push_extern_process is affected as well.
 */
static tree
parse_formparlist ()
{
  tree list = NULL_TREE;
  if (PEEK_TOKEN() == RPRN)
    return NULL_TREE;
  for (;;)
    {
      list = chainon (list, parse_formpar ());
      if (! check_token (COMMA))
	break;
    }
  return list;
}

static tree
parse_opt_result_spec ()
{
  tree mode;
  int is_nonref, is_loc, is_dynamic;
  if (!check_token (RETURNS))
    return void_type_node;
  expect (LPRN, "expected '(' after RETURNS");
  mode = parse_mode ();
  is_nonref = check_token (NONREF);
  is_loc = check_token (LOC);
  is_dynamic = check_token (DYNAMIC);
  if (is_nonref && !is_loc)
    error ("NONREF specific without LOC in result attribute");
  if (is_dynamic && !is_loc)
    error ("DYNAMIC specific without LOC in result attribute");
  mode = get_type_of (mode);
  if (is_loc && ! ignoring)
    mode = build_chill_reference_type (mode);
  expect (RPRN, "expected ')' after RETURNS");
  return mode;
}

static tree
parse_opt_except ()
{
  tree list = NULL_TREE;
  if (!check_token (EXCEPTIONS))
    return NULL_TREE;
  expect (LPRN, "expected '(' after EXCEPTIONS");
  do
    {
      tree except_name = parse_name_string ();
      tree name;
      for (name = list; name != NULL_TREE; name = TREE_CHAIN (name))
	if (TREE_VALUE (name) == except_name && pass == 1)
	  {
	    error ("exception names must be unique");
	    break;
	  }
      if (name == NULL_TREE && !ignoring)
	list = tree_cons (NULL_TREE, except_name, list); 
    } while (check_token (COMMA));
  expect (RPRN, "expected ')' after EXCEPTIONS");
  return list;
}

static tree
parse_opt_recursive ()
{
  if (check_token (RECURSIVE))
    return ridpointers[RID_RECURSIVE];
  else
    return NULL_TREE;
}

static tree
parse_procedureattr ()
{
  tree generality;
  tree optrecursive;
  switch (PEEK_TOKEN ())
    {
    case GENERAL:
      FORWARD_TOKEN ();
      generality = ridpointers[RID_GENERAL];
      break;
    case SIMPLE:
      FORWARD_TOKEN ();
      generality = ridpointers[RID_SIMPLE];
      break;
    case INLINE:
      FORWARD_TOKEN ();
      generality = ridpointers[RID_INLINE];
      break;
    default:
      generality = NULL_TREE;
    }
  optrecursive = parse_opt_recursive ();
  if (pass != 1)
    return NULL_TREE;
  if (generality)
    generality = build_tree_list (NULL_TREE, generality);
  if (optrecursive)
    generality = tree_cons (NULL_TREE, optrecursive, generality);
  return generality;
}

/* Parse the body and last part of a procedure or process definition. */

static void
parse_proc_body (name, exceptions)
     tree name;
     tree exceptions;
{
  int save_proc_action_level = proc_action_level;
  proc_action_level = action_nesting_level;
  if (exceptions != NULL_TREE)
    /* set up a handler for reraising exceptions */
    push_handler ();
  push_action ();
  define__PROCNAME__ ();
  parse_body ();
  proc_action_level = save_proc_action_level;
  expect (END, "'END' was expected here");
  parse_opt_handler ();
  if (exceptions != NULL_TREE)
    chill_reraise_exceptions (exceptions);
  parse_opt_end_label_semi_colon (name);
  end_function ();
}

static void
parse_procedure_definition (in_spec_module)
     int in_spec_module;
{
  int save_ignoring = ignoring;
  tree name = parse_defining_occurrence ();
  tree params, result, exceptlist, attributes;
  int save_chill_at_module_level = chill_at_module_level;
  chill_at_module_level = 0;
  if (!in_spec_module)
    ignoring = pass == 2;
  require (COLON); require (PROC);
  expect (LPRN, "missing '(' after PROC");
  params = parse_formparlist ();
  expect (RPRN, "missing ')' in PROC");
  result = parse_opt_result_spec ();
  exceptlist = parse_opt_except ();
  attributes = parse_procedureattr ();
  ignoring = save_ignoring;
  if (in_spec_module)
    {
      expect (END, "missing 'END'");
      parse_opt_end_label_semi_colon (name);
      push_extern_function (name, result, params, exceptlist, 0);
      return;
    }
  push_chill_function_context ();
  start_chill_function (name, result, params, exceptlist, attributes);
  current_module->procedure_seen = 1; 
  parse_proc_body (name, TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)));
  chill_at_module_level = save_chill_at_module_level;
}

static tree
parse_processpar ()
{
  tree names = parse_defining_occurrence_list ();
  tree mode = parse_mode ();
  tree paramattr = parse_param_attr ();

  if (names && TREE_CODE (names) == IDENTIFIER_NODE)
    names = build_tree_list (NULL_TREE, names);
  return tree_cons (tree_cons (paramattr, mode, NULL_TREE), names, NULL_TREE);
}

static tree
parse_processparlist ()
{
  tree list = NULL_TREE;
  if (PEEK_TOKEN() == RPRN)
    return NULL_TREE;
  for (;;)
    {
      list = chainon (list, parse_processpar ());
      if (! check_token (COMMA))
	break;
    }
  return list;
}

static void
parse_process_definition (in_spec_module)
     int in_spec_module;
{
  int save_ignoring = ignoring;
  tree name = parse_defining_occurrence ();
  tree params;
  tree tmp;
  if (!in_spec_module)
    ignoring = 0;
  require (COLON); require (PROCESS);
  expect (LPRN, "missing '(' after PROCESS");
  params = parse_processparlist ();
  expect (RPRN, "missing ')' in PROCESS");
  ignoring = save_ignoring;
  if (in_spec_module)
    {
      expect (END, "missing 'END'");
      parse_opt_end_label_semi_colon (name);
      push_extern_process (name, params, NULL_TREE, 0);
      return;
    }
  tmp = build_process_header (name, params);
  parse_proc_body (name, NULL_TREE);
  build_process_wrapper (name, tmp);
}

static void
parse_signal_definition ()
{
  tree signame = parse_defining_occurrence ();
  tree modes = NULL_TREE;
  tree dest = NULL_TREE;

  if (check_token (EQL))
    {
      expect (LPRN, "missing '(' after 'SIGNAL <name> ='");
      for (;;)
	{
	  tree mode = parse_mode ();
	  modes = tree_cons (NULL_TREE, mode, modes);
	  if (! check_token (COMMA))
	    break;
	}
      expect (RPRN, "missing ')'");
      modes = nreverse (modes);
    }

  if (check_token (TO))
    {
      tree decl;
      int save_ignoring = ignoring;
      ignoring = 0;
      decl = parse_name ();
      ignoring = save_ignoring;
      if (pass > 1)
	{
	  if (decl == NULL_TREE
	      || TREE_CODE (decl) == ERROR_MARK
	      || TREE_CODE (decl) != FUNCTION_DECL
	      || !CH_DECL_PROCESS (decl))
	    error ("must specify a PROCESS name");
	  else
	    dest = decl; 
	}
    }

  if (! global_bindings_p ())
    error ("SIGNAL must be in global reach");
  else
    {
      tree struc =  build_signal_struct_type (signame, modes, dest);
      tree decl = 
	generate_tasking_code_variable (signame, 
					&signal_code, 
					current_module->is_spec_module);
      /* remember the code variable in the struct type */
      DECL_TASKING_CODE_DECL (struc) = (struct lang_decl *)decl;
      CH_DECL_SIGNAL (struc) = 1;
      add_taskstuff_to_list (decl, "_TT_Signal", 
			     current_module->is_spec_module ?
			     NULL_TREE : signal_code, struc, NULL_TREE);
    }

}

static void
parse_signal_definition_statement ()
{
  int save_ignoring = ignoring;
  ignoring = pass == 2;
  require (SIGNAL);
  for (;;)
    {
      parse_signal_definition ();
      if (! check_token (COMMA))
	break;
      if (PEEK_TOKEN () == SC)
	{
	  error ("syntax error while parsing signal definition statement");
	  break;
	}
    }
  parse_semi_colon ();
  ignoring = save_ignoring;
}

static int
parse_definition (in_spec_module)
     int in_spec_module;
{
  switch (PEEK_TOKEN ())
    {
    case NAME:
      if (PEEK_TOKEN1() == COLON)
	{
	  if (PEEK_TOKEN2() == PROC)
	    {
	      parse_procedure_definition (in_spec_module);
	      return 1;
	    }
	  else if (PEEK_TOKEN2() == PROCESS)
	    {
	      parse_process_definition (in_spec_module);
	      return 1;
	    }
	}
      return 0;
    case DCL:
      parse_declaration_statement(in_spec_module);
      break;
    case GRANT:
      parse_grant_statement ();
      break;
    case NEWMODE:
      parse_mode_definition_statement(1);
      break;
    case SC:
      label = NULL_TREE;
      FORWARD_TOKEN();
      return 1;
    case SEIZE:
      parse_seize_statement ();
      break;
    case SIGNAL:
      parse_signal_definition_statement ();
      break;
    case SYN:
      parse_synonym_definition_statement();
      break;
    case SYNMODE:
      parse_mode_definition_statement(0);
      break;
    default:
      return 0;
    }
  return 1;
}

static void
parse_then_clause ()
{
  expect (THEN, "expected 'THEN' after 'IF'");
  if (! ignoring)
    emit_line_note (input_filename, lineno);
  parse_opt_actions ();
}

static void
parse_opt_else_clause ()
{
  while (check_token (ELSIF))
    {
      tree cond = parse_expression ();
      if (! ignoring)
	expand_start_elseif (truthvalue_conversion (cond));
      parse_then_clause ();
    }
  if (check_token (ELSE))
    {
      if (! ignoring)
	{ emit_line_note (input_filename, lineno);
	  expand_start_else (); 
	} 
      parse_opt_actions ();
    }
}

static tree parse_expr_list ()
{
  tree expr = parse_expression ();
  tree list = ignoring ? NULL_TREE : build_tree_list (NULL_TREE, expr);
  while (check_token (COMMA))
    {
      expr = parse_expression ();
      if (! ignoring)
	list = tree_cons (NULL_TREE, expr, list);
    }
  return list;
}

static tree
parse_range_list_clause ()
{
  tree name = parse_opt_name_string (0);
  if (name == NULL_TREE)
    return NULL_TREE;
  while (check_token (COMMA))
    {
      name = parse_name_string ();
    }
  if (check_token (SC))
    {
      sorry ("case range list"); 
      return error_mark_node;
    }
  pushback_token (NAME, name);
  return NULL_TREE;
}

static void
pushback_paren_expr (expr)
     tree expr;
{
  if (pass == 1 && !ignoring)
    expr = build1 (PAREN_EXPR, NULL_TREE, expr);
  pushback_token (EXPR, expr);
}

/* Matches: <case label> */

static tree
parse_case_label ()
{
  tree expr;
  if (check_token (ELSE))
    return case_else_node;
  /* Does this also handle the case of a mode name?  FIXME */
  expr = parse_expression ();
  if (check_token (COLON))
    {
      tree max_expr = parse_expression ();
      if (! ignoring)
	expr = build (RANGE_EXPR, NULL_TREE, expr, max_expr);
    }
  return expr;
}

/* Parses:  <case_label_list>
   Fails if not followed by COMMA or COLON.
   If it fails, it backs up if needed, and returns NULL_TREE.
   IN_TUPLE is true if we are parsing a tuple element,
   and 0 if we are parsing a case label specification. */

static tree
parse_case_label_list (selector, in_tuple)
     tree selector;
     int in_tuple;
{
  tree expr, list;
  if (! check_token (LPRN))
    return NULL_TREE;
  if (check_token (MUL))
    {
      expect (RPRN, "missing ')' after '*' case label list");
      if (ignoring)
	return integer_zero_node;
      expr = build (RANGE_EXPR, NULL_TREE, NULL_TREE, NULL_TREE);
      expr = build_tree_list (NULL_TREE, expr);
      return expr;
    }
  expr = parse_case_label ();
  if (check_token (RPRN))
    {
      if ((in_tuple || PEEK_TOKEN () != COMMA) && PEEK_TOKEN () != COLON)
	{
	  /* Ooops!  It looks like it was the start of an action or
	     unlabelled tuple element,  and not a case label, so back up. */
	  if (expr != NULL_TREE && TREE_CODE (expr) == RANGE_EXPR)
	    {
	      error ("misplaced colon in case label");
	      expr = error_mark_node;
	    }
	  pushback_paren_expr (expr);
	  return NULL_TREE;
	}
      list = build_tree_list (NULL_TREE, expr);
      if (expr == case_else_node && selector != NULL_TREE)
	ELSE_LABEL_SPECIFIED (selector) = 1;
      return list;
    }
  list = build_tree_list (NULL_TREE, expr);
  if (expr == case_else_node && selector != NULL_TREE)
    ELSE_LABEL_SPECIFIED (selector) = 1;

  while (check_token (COMMA))
    {
      expr = parse_case_label ();
      list = tree_cons (NULL_TREE, expr, list);
      if (expr == case_else_node && selector != NULL_TREE)
	ELSE_LABEL_SPECIFIED (selector) = 1;
    }
  expect (RPRN, "missing ')' at end of case label list");
  return nreverse (list);
}

/* Parses:  <case_label_specification>
   Must be followed by a COLON.
   If it fails, it backs up if needed, and returns NULL_TREE. */

static tree
parse_case_label_specification (selectors)
     tree selectors;
{
  tree list_list = NULL_TREE;
  tree list;
  list = parse_case_label_list (selectors, 0);
  if (list == NULL_TREE)
    return NULL_TREE;
  list_list = build_tree_list (NULL_TREE, list);
  while (check_token (COMMA))
    {
      if (selectors != NULL_TREE)
	selectors = TREE_CHAIN (selectors);
      list = parse_case_label_list (selectors, 0);
      if (list == NULL_TREE)
	{
	  error ("unrecognized case label list after ','");
	  return list_list;
	}
      list_list = tree_cons (NULL_TREE, list, list_list);
    }
  return nreverse (list_list);
}

static void
parse_single_dimension_case_action (selector)
     tree selector;
{
  int  no_completeness_check = 0;

/* The case label/action toggle.  It is 0 initially, and when an action
   was last seen.  It is 1 integer_zero_node when a label was last seen. */
  int caseaction_flag = 0;

  if (! ignoring)
    {
      expand_exit_needed = 0;
      selector = check_case_selector (selector);
      expand_start_case (1, selector, TREE_TYPE (selector), "CASE statement");
      push_momentary ();
    }

  for (;;)
    {
      tree label_spec = parse_case_label_specification (selector);
      if (label_spec != NULL_TREE)
	{
	  expect (COLON, "missing ':' in case alternative");
	  if (! ignoring)
	    {
              no_completeness_check |= chill_handle_single_dimension_case_label (
                selector, label_spec, &expand_exit_needed, &caseaction_flag);
	    }
	}
      else if (parse_action ())
	{
	  expand_exit_needed = 1; 
	  caseaction_flag = 0;
	}
      else
	break;
    }

  if (! ignoring)
    {
      if (expand_exit_needed || caseaction_flag == 1)
	expand_exit_something (); 
    }
  if (check_token (ELSE))
    {
      if (! ignoring)
	  chill_handle_case_default ();
      parse_opt_actions ();
      if (! ignoring)
	{
	  emit_line_note (input_filename, lineno); 
	  expand_exit_something (); 
	}
    }
  else if (! ignoring && TREE_CODE (selector) != ERROR_MARK &&
	   ! no_completeness_check)
    check_missing_cases (TREE_TYPE (selector));

  expect (ESAC, "missing 'ESAC' after 'CASE'");
  if (! ignoring)
    {
      expand_end_case (selector);
      pop_momentary (); 
    }
}

static void
parse_multi_dimension_case_action (selector)
     tree selector;
{
  struct rtx_def *begin_test_label = 0, *end_case_label = 0, *new_label;
  tree action_labels = NULL_TREE;
  tree tests = NULL_TREE;
  int  save_lineno = lineno;
  char *save_filename = input_filename;

  /* We can't compute the range of an (ELSE) label until all of the CASE
     label specifications have been seen, however, the code for the actions
     between them is generated on the fly. We can still generate everything in
     one pass is we use the following form:

     Compile a CASE of the form

       case S1,...,Sn of
         (X11),...,(X1n): A1;
               ...
         (Xm1),...,(Xmn): Am;
         else             Ae;
       esac;

     into:

       goto L0;
       L1:   A1;  goto L99;
          ...
       Lm:   Am;  goto L99;
       Le:   Ae;  goto L99;
       L0:
       T1 := s1; ...; Tn := Sn;
       if (T1 = X11 and ... and Tn = X1n) GOTO L1;
          ...
       if (T1 = Xm1 and ... and Tn = Xmn) GOTO Lm;
       GOTO Le;
       L99;
   */

  if (! ignoring)
    {
      selector = check_case_selector_list (selector);
      begin_test_label = gen_label_rtx ();
      end_case_label   = gen_label_rtx ();
      emit_jump (begin_test_label);
    }

  for (;;)
    {
      tree label_spec = parse_case_label_specification (selector);
      if (label_spec != NULL_TREE)
	{
	  expect (COLON, "missing ':' in case alternative");
	  if (! ignoring)
	    {
	      tests = tree_cons (label_spec, NULL_TREE, tests);

	      if (action_labels != NULL_TREE)
		emit_jump (end_case_label);

	      new_label = gen_label_rtx ();
	      emit_label (new_label);
	      emit_line_note (input_filename, lineno);
	      action_labels = tree_cons (NULL_TREE, NULL_TREE, action_labels);
              TREE_CST_RTL (action_labels) = new_label;
	    }
	}
      else if (! parse_action ())
	{
	  if (action_labels != NULL_TREE)
	    emit_jump (end_case_label);
	  break;
	}
    }

  if (check_token (ELSE))
    {
      if (! ignoring)
	{
	  new_label = gen_label_rtx ();
	  emit_label (new_label);
	  emit_line_note (input_filename, lineno);
	  action_labels = tree_cons (NULL_TREE, NULL_TREE, action_labels);
	  TREE_CST_RTL (action_labels) = new_label;
	}
      parse_opt_actions ();
      if (! ignoring)
	emit_jump (end_case_label);
    }

  expect (ESAC, "missing 'ESAC' after 'CASE'");

  if (! ignoring)
    {
      emit_label (begin_test_label);
      emit_line_note (save_filename, save_lineno);
      if (tests != NULL_TREE)
	{
	  tree cond;
	  tests = nreverse (tests);
	  action_labels = nreverse (action_labels);
	  compute_else_ranges (selector, tests);

	  cond = build_multi_case_selector_expression (selector, TREE_PURPOSE (tests));
	  expand_start_cond (truthvalue_conversion (cond), label ? 1 : 0);
	  emit_jump (TREE_CST_RTL (action_labels));

	  for (tests = TREE_CHAIN (tests), action_labels = TREE_CHAIN (action_labels);
	       tests != NULL_TREE && action_labels != NULL_TREE;
	       tests = TREE_CHAIN (tests), action_labels = TREE_CHAIN (action_labels))
	    {
	      cond =
		build_multi_case_selector_expression (selector, TREE_PURPOSE (tests));
	      expand_start_elseif (truthvalue_conversion (cond));
	      emit_jump (TREE_CST_RTL (action_labels));
	    }
	  if (action_labels != NULL_TREE)
	    {
	      expand_start_else (); 
	      emit_jump (TREE_CST_RTL (action_labels));
	    }
	  expand_end_cond (); 
	}
      emit_label (end_case_label);
    }
}

static void
parse_case_action (label)
     tree label;
{
  tree selector;
  int  multi_dimension_case = 0;

  require (CASE);
  selector = parse_expr_list ();
  selector = nreverse (selector);
  expect (OF, "missing 'OF' after 'CASE'");
  parse_range_list_clause ();

  PUSH_ACTION;
  if (label)
    pushlevel (1);

  if (! ignoring)
    {
      expand_exit_needed = 0;
      if (TREE_CODE (selector) == TREE_LIST)
	{
	  if (TREE_CHAIN (selector) != NULL_TREE)
            multi_dimension_case = 1;
          else
	    selector = TREE_VALUE (selector);
	}
    }

  /* We want to use the regular CASE support for the single dimension case. The
     multi dimension case requires different handling. Note that when "ignoring"
     is true we parse using the single dimension code. This is OK since it will
     still parse correctly. */
  if (multi_dimension_case)
    parse_multi_dimension_case_action (selector);
  else
    parse_single_dimension_case_action (selector);

  if (label)
    {
      possibly_define_exit_label (label);
      poplevel (0, 0, 0);
    }
}

/* Matches: [ <asm_operand> { "," <asm_operand> }* ],
   where <asm_operand> = STRING '(' <expression> ')'
   These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */

static tree
parse_asm_operands ()
{
  tree list = NULL_TREE;
  if (PEEK_TOKEN () != STRING)
    return NULL_TREE;
  for (;;)
    {
      tree string, expr;
      if (PEEK_TOKEN () != STRING)
	{
	  error ("bad ASM operand");
	  return list;
	}
      string = PEEK_TREE();
      FORWARD_TOKEN ();
      expect (LPRN, "missing '(' in ASM operand");
      expr = parse_expression ();
      expect (RPRN, "missing ')' in ASM operand");
      list = tree_cons (string, expr, list);
      if (! check_token (COMMA))
	break;
    }
  return nreverse (list);
}

/* Matches:  STRING { ',' STRING }* */

static tree
parse_asm_clobbers ()
{
  tree list = NULL_TREE;
  for (;;)
    {
      tree string;
      if (PEEK_TOKEN () != STRING)
	{
	  error ("bad ASM operand");
	  return list;
	}
      string = PEEK_TREE();
      FORWARD_TOKEN ();
      list = tree_cons (NULL_TREE, string, list);
      if (! check_token (COMMA))
	break;
    }
  return list;
}

static void
ch_expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     char *filename;
     int line;
{
  int noutputs = list_length (outputs);
  register int i;
  /* o[I] is the place that output number I should be written.  */
  register tree *o = (tree *) alloca (noutputs * sizeof (tree));
  register tree tail;

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);
  if (TREE_CODE (string) != STRING_CST)
    {
      error ("asm template is not a string constant");
      return;
    }

  /* Record the contents of OUTPUTS before it is modified.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    o[i] = TREE_VALUE (tail);

#if 0
  /* Perform default conversions on array and function inputs.  */
  /* Don't do this for other types--
     it would screw up operands expected to be in memory.  */
  for (i = 0, tail = inputs; tail; tail = TREE_CHAIN (tail), i++)
    if (TREE_CODE (TREE_TYPE (TREE_VALUE (tail))) == ARRAY_TYPE
	|| TREE_CODE (TREE_TYPE (TREE_VALUE (tail))) == FUNCTION_TYPE)
      TREE_VALUE (tail) = default_conversion (TREE_VALUE (tail));
#endif

  /* Generate the ASM_OPERANDS insn;
     store into the TREE_VALUEs of OUTPUTS some trees for
     where the values were actually stored.  */
  expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line);

  /* Copy all the intermediate outputs into the specified outputs.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      if (o[i] != TREE_VALUE (tail))
	{
	  expand_expr (build_chill_modify_expr (o[i], TREE_VALUE (tail)),
		       0, VOIDmode, 0);
	  free_temp_slots ();
	}
      /* Detect modification of read-only values.
	 (Otherwise done by build_modify_expr.)  */
      else
	{
	  tree type = TREE_TYPE (o[i]);
	  if (TYPE_READONLY (type)
	      || ((TREE_CODE (type) == RECORD_TYPE
		   || TREE_CODE (type) == UNION_TYPE)
		  && TYPE_FIELDS_READONLY (type)))
	    warning ("readonly location modified by 'asm'");
	}
    }

  /* Those MODIFY_EXPRs could do autoincrements.  */
  emit_queue ();
}

static void
parse_asm_action ()
{
  tree insn;
  require (ASM_KEYWORD);
  expect (LPRN, "missing '('");
  PUSH_ACTION;
  if (!ignoring)
    emit_line_note (input_filename, lineno);
  insn = parse_expression ();
  if (check_token (COLON))
    {
      tree output_operand, input_operand, clobbered_regs;
      output_operand = parse_asm_operands ();
      if (check_token (COLON))
	input_operand = parse_asm_operands ();
      else
	input_operand = NULL_TREE;
      if (check_token (COLON))
	clobbered_regs = parse_asm_clobbers ();
      else
	clobbered_regs = NULL_TREE;
      expect (RPRN, "missing ')'");
      if (!ignoring)
	ch_expand_asm_operands (insn, output_operand, input_operand,
				clobbered_regs, FALSE,
				input_filename, lineno);
    }
  else
    {
      expect (RPRN, "missing ')'");
      STRIP_NOPS (insn);
      if (ignoring) { }
      else if ((TREE_CODE (insn) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (insn, 0)) == STRING_CST)
	  || TREE_CODE (insn) == STRING_CST)
	expand_asm (insn);
      else
	error ("argument of `asm' is not a constant string");
    }
}

static void
parse_begin_end_block (label)
     tree label;
{
  require (BEGINTOKEN);
#if 0
  /* don't make a linenote at BEGIN */
  INIT_ACTION;
#endif
  pushlevel (1);
  if (! ignoring)
    {
      clear_last_expr ();
      push_momentary ();
      expand_start_bindings (label ? 1 : 0); 
    }
  push_handler ();
  parse_body ();
  expect (END, "missing 'END'");
  /* Note that the opthandler comes before the poplevel
     - hence a handler is in the scope of the block. */
  parse_opt_handler ();
  possibly_define_exit_label (label);
  if (! ignoring)
    { 
      emit_line_note (input_filename, lineno);
      expand_end_bindings (getdecls (), kept_level_p (), 0);
    }
  poplevel (kept_level_p (), 0, 0);
  if (! ignoring)
    pop_momentary (); 
  parse_opt_end_label_semi_colon (label);
}

static void
parse_if_action (label)
     tree label;
{
  tree cond;
  require (IF);
  PUSH_ACTION;
  cond = parse_expression ();
  if (label)
    pushlevel (1);
  if (! ignoring)
    { 
      expand_start_cond (truthvalue_conversion (cond),
			 label ? 1 : 0); 
    }
  parse_then_clause ();
  parse_opt_else_clause ();
  expect (FI, "expected 'FI' after 'IF'");
  if (! ignoring)
    { 
      emit_line_note (input_filename, lineno);
      expand_end_cond (); 
    }
  if (label)
    {
      possibly_define_exit_label  (label);
      poplevel (0, 0, 0);
    }
}

/* Matches:  <iteration>  (as in a <for control>). */

static void
parse_iteration ()
{
  tree loop_counter = parse_defining_occurrence ();
  if (check_token (ASGN))
    {
      tree start_value = parse_expression ();
      tree step_value
	= check_token (BY) ? parse_expression () : NULL_TREE;
      int going_down = check_token (DOWN);
      tree end_value;
      if (check_token (TO))
	end_value = parse_expression ();
      else
	{
	  error ("expected 'TO' in step enumeration");
	  end_value = error_mark_node;
	}
      if (!ignoring)
	build_loop_iterator (loop_counter, start_value, step_value,
			     end_value, going_down, 0, 0);
    }
  else
    {
      int going_down = check_token (DOWN);
      tree expr;
      if (check_token (IN))
	expr = parse_expression ();
      else
	{
	  error ("expected 'IN' in FOR control here");
	  expr = error_mark_node;
	}
      if (!ignoring)
	{
	  tree low_bound, high_bound;
	  if (expr && TREE_CODE (expr) == TYPE_DECL)
	    {
	      expr = TREE_TYPE (expr);
	      /* FIXME: expr must be an array or powerset */
	      low_bound = convert (expr, TYPE_MIN_VALUE (expr));
	      high_bound = convert (expr, TYPE_MAX_VALUE (expr));
	    }
	  else
	    {
	      low_bound = expr;
	      high_bound = NULL_TREE;
	    }
	  build_loop_iterator (loop_counter, low_bound,
			       NULL_TREE, high_bound,
			       going_down, 1, 0);
	}
    }
}

/* Matches: '(' <event list> ')' ':'.
   Or; returns NULL_EXPR. */

static tree
parse_delay_case_event_list ()
{
  tree event_list = NULL_TREE;
  tree event;
  if (! check_token (LPRN))
    return NULL_TREE;
  event = parse_expression ();
  if (PEEK_TOKEN () == ')' && PEEK_TOKEN1 () != ':')
    {
      /* Oops. */
      require (RPRN);
      pushback_paren_expr (event);
      return NULL_TREE;
    }
  for (;;)
    {
      if (! ignoring)
	event_list = tree_cons (NULL_TREE, event, event_list);
      if (! check_token (COMMA))
	break;
      event = parse_expression ();
    }
  expect (RPRN, "missing ')'");
  expect (COLON, "missing ':'");
  return ignoring ? error_mark_node : event_list;
}

static void
parse_delay_case_action (label)
     tree label;
{
  tree label_cnt = NULL_TREE, set_location, priority;
  tree combined_event_list = NULL_TREE;
  require (DELAY);
  require (CASE);
  PUSH_ACTION;
  pushlevel (1);
  expand_exit_needed = 0;
  if (check_token (SET))
    {
      set_location = parse_expression ();
      parse_semi_colon ();
    }
  else
    set_location = NULL_TREE;
  if (check_token (PRIORITY))
    {
      priority = parse_expression ();
      parse_semi_colon ();
    }
  else
    priority = NULL_TREE;
  if (! ignoring)
    label_cnt = build_delay_case_start (set_location, priority);
  for (;;)
    {
      tree event_list = parse_delay_case_event_list ();
      if (event_list)
	{
	  if (! ignoring )
	    { 
	      int if_or_elseif = combined_event_list == NULL_TREE;
	      build_delay_case_label (event_list, if_or_elseif);  
	      combined_event_list = chainon (combined_event_list, event_list);
	    }
	}
      else if (parse_action ())
	{
	  if (! ignoring)
	    {
	      expand_exit_needed = 1;
	      if (combined_event_list == NULL_TREE)
		error ("missing DELAY CASE alternative");
	    }
	}
      else
	break;
    }
  expect (ESAC, "missing 'ESAC' in DELAY CASE'");
  if (! ignoring)
    build_delay_case_end (combined_event_list);
  possibly_define_exit_label (label);
  poplevel (0, 0, 0); 
}

static void
parse_do_action (label)
     tree label;
{
  tree condition;
  int token;
  require (DO);
  if (check_token (WITH))
    {
      tree list = NULL_TREE;
      for (;;)
	{
	  tree name = parse_primval ();
	  if (! ignoring && TREE_CODE (name) != ERROR_MARK)
	    {
	      if (TREE_CODE (TREE_TYPE (name)) == REFERENCE_TYPE)
		name = convert (TREE_TYPE (TREE_TYPE (name)), name);
	      else
		{
		  int is_loc = chill_location (name);
		  if (is_loc == 1) /* This is probably not possible */
		    warning ("non-referable location in DO WITH");
		  
		  if (is_loc > 1)
		    name = build_chill_arrow_expr (name, 1);
		  name = decl_temp1 (get_identifier ("__with_element"),
				     TREE_TYPE (name),
				     0, name, 0, 0);
		  if (is_loc > 1)
		    name = build_chill_indirect_ref (name, NULL_TREE, 0);
		  
		}
	      if (TREE_CODE (TREE_TYPE (name)) != RECORD_TYPE)
		error ("WITH element must be of STRUCT mode");
	      else
		list = tree_cons (NULL_TREE, name, list);
	    }
	  if (! check_token (COMMA))
	    break;
	}
      pushlevel (1);
      push_action ();
      for (list = nreverse (list); list != NULL_TREE; list = TREE_CHAIN (list))
	shadow_record_fields (TREE_VALUE (list));

      parse_semi_colon ();
      parse_opt_actions ();
      expect (OD, "missing 'OD' in 'DO WITH'");
      if (! ignoring)
	emit_line_note (input_filename, lineno);
      possibly_define_exit_label (label);
      parse_opt_handler ();
      parse_opt_end_label_semi_colon (label);
      poplevel (0, 0, 0); 
      return;
    }
  token = PEEK_TOKEN();
  if (token != FOR && token != WHILE)
    {
      push_handler ();
      parse_opt_actions ();
      expect (OD, "Missing 'OD' after 'DO'");
      parse_opt_handler ();
      parse_opt_end_label_semi_colon (label);
      return;
    }
  if (! ignoring)
    emit_line_note (input_filename, lineno);
  push_loop_block ();
  if (check_token (FOR))
    {
      if (check_token (EVER))
	{
	  if (!ignoring)
	    build_loop_iterator (NULL_TREE, NULL_TREE,
				 NULL_TREE, NULL_TREE,
				 0, 0, 1);
	}
      else
	{
	  parse_iteration ();
	  while (check_token (COMMA))
	    parse_iteration ();
	}
    }
  else if (!ignoring)
    build_loop_iterator (NULL_TREE, NULL_TREE,
			 NULL_TREE, NULL_TREE,
			 0, 0, 1);
       
  begin_loop_scope ();
  if (! ignoring)
    build_loop_start (label);
  condition = check_token (WHILE) ? parse_expression () : NULL_TREE;
  if (! ignoring)
    top_loop_end_check (condition);
  parse_semi_colon ();
  parse_opt_actions ();
  if (! ignoring)
    build_loop_end (); 
  expect (OD, "Missing 'OD' after 'DO'");
  /* Note that the handler is inside the reach of the DO. */
  parse_opt_handler ();
  end_loop_scope (label);
  pop_loop_block ();
  parse_opt_end_label_semi_colon (label);
}

/* Matches: '(' <signal name> [ 'IN' <defining occurrence list> ']' ')' ':'
   or: '(' <buffer location> IN (defining occurrence> ')' ':'
   or: returns NULL_TREE. */

static tree
parse_receive_spec ()
{
  tree val;
  tree name_list = NULL_TREE;
  if (!check_token (LPRN))
    return NULL_TREE;
  val = parse_primval ();
  if (check_token (IN))
    {
#if 0
      if (flag_local_loop_counter)
	name_list = parse_defining_occurrence_list ();
      else
#endif
	{
	  for (;;)
	    {
	      tree loc = parse_primval ();
	      if (! ignoring)
		name_list = tree_cons (NULL_TREE, loc, name_list);
	      if (! check_token (COMMA))
		break;
	    }
	}
    }
  if (! check_token (RPRN))
    {
      error ("missing ')' in signal/buffer receive alternative");
      return NULL_TREE;
    }
  if (check_token (COLON))
    {
      if (ignoring || val == NULL_TREE || TREE_CODE (val) == ERROR_MARK)
	return error_mark_node;
      else
	return build_receive_case_label (val, name_list);
    }

  /* We saw: '(' <primitive value> ')' not followed by ':'.
     Presumably the start of an action.  Backup and fail. */
  if (name_list != NULL_TREE)
    error ("misplaced 'IN' in signal/buffer receive alternative");
  pushback_paren_expr (val);
  return NULL_TREE;
}

/* To understand the code generation for this, see ch-tasking.c,
   and the 2-page comments preceding the
   build_chill_receive_case_start () definition. */

static void
parse_receive_case_action (label)
     tree label;
{
  tree instance_location;
  tree have_else_actions;
  int spec_seen = 0;
  tree alt_list = NULL_TREE;
  require (RECEIVE);
  require (CASE);
  push_action ();
  pushlevel (1);
  if (! ignoring)
    {
      expand_exit_needed = 0;
    }

  if (check_token (SET))
    {
      instance_location = parse_expression ();
      parse_semi_colon ();
    }
  else
    instance_location = NULL_TREE;
  if (! ignoring)
    instance_location = build_receive_case_start (instance_location);

  for (;;)
    {
      tree receive_spec = parse_receive_spec ();
      if (receive_spec)
	{
	  if (! ignoring)
	    alt_list = tree_cons (NULL_TREE, receive_spec, alt_list);
	  spec_seen++;
	}
      else if (parse_action ())
	{
	  if (! spec_seen && pass == 1)
	    error ("missing RECEIVE alternative");
	  if (! ignoring)
	    expand_exit_needed = 1;
	  spec_seen = 1;
	}
      else
	break;
    }
  if (check_token (ELSE))
    {
      if (! ignoring)
	{
	  emit_line_note (input_filename, lineno); 
	  if (build_receive_case_if_generated ())
	    expand_start_else ();
	}
      parse_opt_actions ();
      have_else_actions = integer_one_node;
    }
  else
    have_else_actions = integer_zero_node;
  expect (ESAC, "missing 'ESAC' matching 'RECEIVE CASE'");
  if (! ignoring)
    {
      build_receive_case_end (nreverse (alt_list), have_else_actions);
    }
  possibly_define_exit_label (label);
  poplevel (0, 0, 0); 
}

static void
parse_send_action ()
{
  tree signal = NULL_TREE;
  tree buffer = NULL_TREE;
  tree value_list;
  tree with_expr, to_expr, priority;
  require (SEND);
  /* The tricky part is distinguishing between a SEND buffer action,
     and a SEND signal action. */
  if (pass != 2 || PEEK_TOKEN () != NAME)
    {
      /* If this is pass 2, it's a SEND buffer action.
	 If it's pass 1, we don't care. */
      buffer = parse_primval ();
    }
  else
    {
      /* We have to specifically check for signalname followed by
	 a '(', since we allow a signalname to be used (syntactically)
	 as a "function". */
      tree name = parse_name ();
      if (TREE_CODE (name) == TYPE_DECL && CH_DECL_SIGNAL (name))
	signal = name; /* It's a SEND signal action! */
      else
	{
	  /* It's not a legal SEND signal action.
	     Back up and try as a SEND buffer action. */
	  pushback_token (EXPR, name);
	  buffer = parse_primval ();
	}
    }
  if (check_token (LPRN))
    {
      value_list = NULL_TREE;
      for (;;)
	{
	  tree expr = parse_untyped_expr ();
	  if (! ignoring)
	    value_list = tree_cons (NULL_TREE, expr, value_list);
	  if (! check_token (COMMA))
	    break;
	}
      value_list = nreverse (value_list);
      expect (RPRN, "missing ')'");
    }
  else
    value_list = NULL_TREE;
  if (check_token (WITH))
    with_expr = parse_expression ();
  else
    with_expr = NULL_TREE;
  if (check_token (TO))
    to_expr = parse_expression ();
  else
    to_expr = NULL_TREE;
  if (check_token (PRIORITY))
    priority = parse_expression ();
  else
    priority = NULL_TREE;
  PUSH_ACTION;
  if (ignoring)
    return;

  if (signal)
    { /* It's a <send signal action>! */
      tree sigdesc = build_signal_descriptor (signal, value_list);
      if (sigdesc != NULL_TREE && TREE_CODE (sigdesc) != ERROR_MARK)
	{
	  tree sendto = to_expr ? to_expr : IDENTIFIER_SIGNAL_DEST (signal);
	  expand_send_signal (sigdesc, with_expr,
			      sendto, priority, DECL_NAME (signal));
	}
    }
  else
    {
      /* all checks are done in expand_send_buffer */
      expand_send_buffer (buffer, value_list, priority, with_expr, to_expr);
    }
}

static void
parse_start_action ()
{
  tree name, copy_number, param_list, startset;
  require (START);
  name = parse_name_string ();
  expect (LPRN, "missing '(' in START action");
  PUSH_ACTION;
  /* copy number is a required parameter */
  copy_number = parse_expression ();
  if (!ignoring
      && (copy_number == NULL_TREE 
	  || TREE_CODE (copy_number) == ERROR_MARK
	  || TREE_CODE (TREE_TYPE (copy_number)) != INTEGER_TYPE))
    {
      error ("PROCESS copy number must be integer");
      copy_number = integer_zero_node;
    }
  if (check_token (COMMA))
    param_list = parse_expr_list (); /* user parameters */
  else
    param_list = NULL_TREE;
  expect (RPRN, "missing ')'");
  startset = check_token (SET) ? parse_primval () : NULL;
  build_start_process (name, copy_number, param_list, startset);
}

static void
parse_opt_actions ()
{
  while (parse_action ()) ;
}

static int
parse_action ()
{
  tree label = NULL_TREE;
  tree expr, rhs, loclist;
  enum tree_code op;

  if (current_function_decl == global_function_decl
      && PEEK_TOKEN () != SC
      && PEEK_TOKEN () != END)
    seen_action = 1, build_constructor = 1;

  if (PEEK_TOKEN () == NAME && PEEK_TOKEN1 () == COLON)
    {
      label = parse_defining_occurrence ();
      require (COLON);
      INIT_ACTION;
      define_label (input_filename, lineno, label);
    }

  switch (PEEK_TOKEN ())
    {
    case AFTER:
      {
	int delay;
	require (AFTER);
	expr = parse_primval ();
	delay = check_token (DELAY);
	expect (IN, "missing 'IN'");
	push_action ();
	pushlevel (1);
	build_after_start (expr, delay);
	parse_opt_actions ();
	expect (TIMEOUT, "missing 'TIMEOUT'");
	build_after_timeout_start ();
	parse_opt_actions ();
	expect (END, "missing 'END'");
	build_after_end ();
	possibly_define_exit_label (label);
	poplevel (0, 0, 0); 
      }
      goto bracketed_action;
    case ASM_KEYWORD:
      parse_asm_action ();
      goto no_handler_action;
    case ASSERT:
      require (ASSERT);
      PUSH_ACTION;
      expr = parse_expression ();
      if (! ignoring)
	{ tree assertfail = ridpointers[(int) RID_ASSERTFAIL];
	  expr = build (TRUTH_ORIF_EXPR, void_type_node, expr,
			build_cause_exception (assertfail, 0));
	  expand_expr_stmt (fold (expr));
	}
      goto handler_action;
    case AT:
      require (AT);
      PUSH_ACTION;
      expr = parse_primval ();
      expect (IN, "missing 'IN'");
      pushlevel (1);
      if (! ignoring)
	build_at_action (expr);
      parse_opt_actions ();
      expect (TIMEOUT, "missing 'TIMEOUT'");
      if (! ignoring)
	expand_start_else ();
      parse_opt_actions ();
      expect (END, "missing 'END'");
      if (! ignoring)
	expand_end_cond ();
      possibly_define_exit_label (label);
      poplevel (0, 0, 0);
      goto bracketed_action;
    case BEGINTOKEN:
      parse_begin_end_block (label);
      return 1;
    case CASE:
      parse_case_action (label);
      goto bracketed_action;
    case CAUSE:
      require (CAUSE);
      expr = parse_name_string ();
      PUSH_ACTION;
      if (! ignoring && TREE_CODE (expr) != ERROR_MARK)
	expand_cause_exception (expr);
      goto no_handler_action;
    case CONTINUE:
      require (CONTINUE);
      expr = parse_expression ();
      PUSH_ACTION;
      if (! ignoring)
	expand_continue_event (expr);
      goto handler_action;
    case CYCLE:
      require (CYCLE);
      PUSH_ACTION;
      expr = parse_primval ();
      expect (IN, "missing 'IN' after 'CYCLE'");
      pushlevel (1);
      /* We a tree list where TREE_VALUE is the label
	 and TREE_PURPOSE is the variable denotes the timeout id. */
      expr = build_cycle_start (expr);
      parse_opt_actions ();
      expect (END, "missing 'END'");
      if (! ignoring)
	build_cycle_end (expr);
      possibly_define_exit_label (label);
      poplevel (0, 0, 0);
      goto bracketed_action;
    case DELAY:
      if (PEEK_TOKEN1 () == CASE)
	{
	  parse_delay_case_action (label);
	  goto bracketed_action;
	}
      require (DELAY);
      PUSH_ACTION;
      expr = parse_primval ();
      rhs = check_token (PRIORITY) ? parse_expression () : NULL_TREE;
      if (! ignoring)
	build_delay_action (expr, rhs);
      goto handler_action;
    case DO:
      parse_do_action (label);
      return 1;
    case EXIT:
      require (EXIT);
      expr = parse_name_string ();
      PUSH_ACTION;
      lookup_and_handle_exit (expr);
      goto no_handler_action;
    case GOTO:
      require (GOTO);
      expr = parse_name_string ();
      PUSH_ACTION;
      lookup_and_expand_goto (expr);
      goto no_handler_action;
    case IF:
      parse_if_action (label);
      goto bracketed_action;
    case RECEIVE:
      if (PEEK_TOKEN1 () != CASE)
	return 0;
      parse_receive_case_action (label);
      goto bracketed_action;
    case RESULT:
      require (RESULT);
      PUSH_ACTION;
      expr = parse_untyped_expr ();
      if (! ignoring)
	chill_expand_result (expr, 1);
      goto handler_action;
    case RETURN:
      require (RETURN);
      PUSH_ACTION;
      expr = parse_opt_untyped_expr ();
      if (! ignoring)
	{
	  /* Do this as RESULT expr and RETURN to get exceptions */
	  chill_expand_result (expr, 0);
	  expand_goto_except_cleanup (proc_action_level);
	  chill_expand_return (NULL_TREE, 0);
	}
      if (expr)
	goto handler_action;
      else
	goto no_handler_action;
    case SC:
      require (SC);
      return 1;
    case SEND:
      parse_send_action ();
      goto handler_action;
    case START:
      parse_start_action ();
      goto handler_action;
    case STOP:
      require (STOP);
      PUSH_ACTION;
      if (! ignoring)
	{ tree func = lookup_name (get_identifier ("__stop_process"));
	  tree result = build_chill_function_call (func, NULL_TREE);
	  expand_expr_stmt (result);
	} 
      goto no_handler_action;
    case CALL:
      require (CALL);
      /* Fall through to here ... */
    case EXPR:
    case LPRN:
    case NAME:
      /* This handles calls and assignments. */
      PUSH_ACTION;
      expr = parse_primval ();
      switch (PEEK_TOKEN ())
	{
	case END:
	  parse_semi_colon ();  /* Emits error message. */
	case ON:
	case SC:
	  if (!ignoring && TREE_CODE (expr) != ERROR_MARK)
	    {
	      if (TREE_CODE (expr) != CALL_EXPR
		  && TREE_TYPE (expr) != void_type_node
		  && ! TREE_SIDE_EFFECTS (expr))
		{
		  if (TREE_CODE (expr) == FUNCTION_DECL)
		    error ("missing parenthesis for procedure call");
		  else
		    error ("expression is not an action");
		  expr = error_mark_node;
		}
	      else
		expand_expr_stmt (expr);
	    }
	  goto handler_action;
	default:
	  loclist
	    = ignoring ? NULL_TREE : build_tree_list (NULL_TREE, expr);
	  while (PEEK_TOKEN () == COMMA)
	    {
	      FORWARD_TOKEN ();
	      expr = parse_primval ();
	      if (!ignoring && TREE_CODE (expr) != ERROR_MARK)
		loclist = tree_cons (NULL_TREE, expr, loclist);
	    }
	}
      switch (PEEK_TOKEN ())
	{
	case OR:	op = BIT_IOR_EXPR;	break;
	case XOR:	op = BIT_XOR_EXPR;	break;
	case ORIF:	op = TRUTH_ORIF_EXPR;	break;
	case AND:	op = BIT_AND_EXPR;	break;
	case ANDIF:	op = TRUTH_ANDIF_EXPR;	break;
	case PLUS:	op = PLUS_EXPR;		break;
	case SUB:	op = MINUS_EXPR;	break;
	case CONCAT:	op = CONCAT_EXPR;	break;
	case MUL:	op = MULT_EXPR;		break;
	case DIV:	op = TRUNC_DIV_EXPR;	break;
	case MOD:	op = FLOOR_MOD_EXPR;	break;
	case REM:	op = TRUNC_MOD_EXPR;	break;

	default:
	  error ("syntax error in action");
	case SC:  case ON:
	case ASGN:	op = NOP_EXPR;		break;
	  ;
	}

      /* Looks like it was an assignment action. */
      FORWARD_TOKEN ();
      if (op != NOP_EXPR)
	expect (ASGN, "expected ':=' here");
      rhs = parse_untyped_expr ();
      if (!ignoring)
	expand_assignment_action (loclist, op, rhs);
      goto handler_action;

    default:
      return 0;
    }

 bracketed_action:
  /* We've parsed a bracketed action. */
  parse_opt_handler ();
  parse_opt_end_label_semi_colon (label);
  return 1;

 no_handler_action:
  if (parse_opt_handler () != NULL_TREE && pass == 1)
    error ("no handler is permitted on this action.");
  parse_semi_colon ();
  return 1;

 handler_action:
  parse_opt_handler ();
  parse_semi_colon ();
  return 1;
}

static void
parse_body ()
{
 again:
  while (parse_definition (0)) ;

  while (parse_action ()) ;

  if (parse_definition (0))
    {
      if (pass == 1)
	pedwarn ("definition follows action");
      goto again;
    }
}

static tree
parse_opt_untyped_expr ()
{
  switch (PEEK_TOKEN ())
    {
    case ON:
    case END:
    case SC:
    case COMMA:
    case COLON:
    case RPRN:
      return NULL_TREE;
    default:
      return parse_untyped_expr ();
    }
}

static tree
parse_call (function)
     tree function;
{
  tree arg1, arg2, arg_list = NULL_TREE;
  enum terminal tok;
  require (LPRN);
  arg1 = parse_opt_untyped_expr ();
  if (arg1 != NULL_TREE)
    {
      tok = PEEK_TOKEN ();
      if (tok == UP || tok == COLON)
	{
	  FORWARD_TOKEN ();
#if 0
	  /* check that arg1 isn't untyped (or mode);*/
#endif
	  arg2 = parse_expression ();
	  expect (RPRN, "expected ')' to terminate slice");
	  if (ignoring)
	    return integer_zero_node;
	  else if (tok == UP)
	    return build_chill_slice_with_length (function, arg1, arg2);
	  else
	    return build_chill_slice_with_range (function, arg1, arg2);
	}
      if (!ignoring)
	arg_list = build_tree_list (NULL_TREE, arg1);
      while (check_token (COMMA))
	{
	  arg2 = parse_untyped_expr ();
	  if (!ignoring)
	    arg_list = tree_cons (NULL_TREE, arg2, arg_list);
	}
    }
     
  expect (RPRN, "expected ')' here");
  return ignoring ? function
    : build_generalized_call (function, nreverse (arg_list));
}

/* Matches:  <field name list>
   Returns:  A list of IDENTIFIER_NODEs (or NULL_TREE if ignoring),
   in reverse order. */

static tree
parse_tuple_fieldname_list ()
{
  tree list = NULL_TREE;
  do
    {
      tree name;
      if (!check_token (DOT))
	{
	  error ("bad tuple field name list");
	  return NULL_TREE;
	}
      name = parse_simple_name_string ();
      list = ignoring ? NULL_TREE : tree_cons (NULL_TREE, name, list);
    }  while (check_token (COMMA));
  return list;
}

/* Returns one or nore TREE_LIST nodes, in reverse order. */

static tree
parse_tuple_element ()
{
  /* The tupleelement chain is built in reverse order,
     and put in forward order when the list is used.  */
  tree value, label;
  if (PEEK_TOKEN () == DOT)
    {
      /* Parse a labelled structure tuple. */
      tree list = parse_tuple_fieldname_list (), field;
      expect (COLON, "missing ':' in tuple");
      value = parse_untyped_expr ();
      if (ignoring)
	return NULL_TREE;
      /* FIXME:  Should use save_expr(value), but that
	 confuses nested calls to digest_init! */
      /* Re-use the list of field names as a list of name-value pairs. */
      for (field = list; field != NULL_TREE; field = TREE_CHAIN (field))
	{ tree field_name = TREE_VALUE (field);
	  TREE_PURPOSE (field) = field_name;
	  TREE_VALUE (field) = value;
	  TUPLE_NAMED_FIELD (field) = 1;
	}
      return list;
    }

  label = parse_case_label_list (NULL_TREE, 1);
  if (label)
    {
      expect (COLON, "missing ':' in tuple");
      value = parse_untyped_expr ();
      if (ignoring || label == NULL_TREE)
	return NULL_TREE;
      if (TREE_CODE (label) != TREE_LIST)
	{
	  error ("invalid syntax for label in tuple");
	  return NULL_TREE;
	}
      else
	{
	  /* FIXME:  Should use save_expr(value), but that
	     confuses nested calls to digest_init! */
	  tree link = label;
	  for (; link != NULL_TREE; link = TREE_CHAIN (link))
	    { tree index = TREE_VALUE (link);
	      if (pass == 1 && TREE_CODE (index) != TREE_LIST)
		index = build1 (PAREN_EXPR, NULL_TREE, index);
	      TREE_VALUE (link) = value;
	      TREE_PURPOSE (link) = index;
	    }
	  return nreverse (label);
	}
    }
  
  value = parse_untyped_expr ();
  if (check_token (COLON))
    {
      /* A powerset range [or possibly a labeled Array?] */
      tree value2 = parse_untyped_expr ();
      return ignoring ? NULL_TREE : build_tree_list (value, value2);
    }
  return ignoring ? NULL_TREE : build_tree_list (NULL_TREE, value);
}

/* Matches:  a COMMA-separated list of tuple elements.
   Returns a list (of TREE_LIST nodes). */
static tree
parse_opt_element_list ()
{
  tree list = NULL_TREE;
  if (PEEK_TOKEN () == RPC)
    return NULL_TREE;
  for (;;)
    {
      tree element = parse_tuple_element ();
      list = chainon (element, list); /* Built in reverse order */
      if (PEEK_TOKEN () == RPC)
	break;
      if (!check_token (COMMA))
	{
	  error ("bad syntax in tuple");
	  return NULL_TREE;
	}
    }
  return nreverse (list);
}

/* Parses: '[' elements ']'
   If modename is non-NULL it prefixed the tuple.  */

static tree
parse_tuple (modename)
     tree modename;
{
  tree list;
  require (LPC);
  list = parse_opt_element_list ();
  expect (RPC, "missing ']' after tuple");
  if (ignoring)
    return integer_zero_node;
  list =  build_nt (CONSTRUCTOR, NULL_TREE, list);
  if (modename == NULL_TREE)
    return list;
  else if (pass == 1)
    TREE_TYPE (list) = modename;
  else if (TREE_CODE (modename) != TYPE_DECL)
    {
      error ("non-mode name before tuple");
      return error_mark_node;
    }
  else
    list = chill_expand_tuple (TREE_TYPE (modename), list);
  return list;
}

static tree
parse_primval ()
{
  tree val;
  switch (PEEK_TOKEN ())
    {
    case NUMBER:
    case FLOATING:
    case STRING:
    case SINGLECHAR:
    case BITSTRING:
    case CONST:
    case EXPR:
      val = PEEK_TREE();
      FORWARD_TOKEN ();
      break;
    case THIS:
      val = build_chill_function_call (PEEK_TREE (), NULL_TREE);
      FORWARD_TOKEN ();
      break;
    case LPRN:
      FORWARD_TOKEN ();
      val = parse_expression ();
      expect (RPRN, "missing right parenthesis");
      if (pass == 1 && ! ignoring)
	val = build1 (PAREN_EXPR, NULL_TREE, val);
      break;
    case LPC:
      val = parse_tuple (NULL_TREE);
      break;
    case NAME:
      val = parse_name ();
      if (PEEK_TOKEN() == LPC)
	val = parse_tuple (val); /* Matched:  <mode_name> <tuple> */
      break;
    default: 
      if (!ignoring)
	error ("invalid expression/location syntax");
      val = error_mark_node;
    }
  for (;;)
    {
      tree name, args;
      switch (PEEK_TOKEN ())
	{
	case DOT:
	  FORWARD_TOKEN ();
	  name = parse_simple_name_string ();
	  val = ignoring ? val : build_chill_component_ref (val, name);
	  continue;
	case ARROW:
	  FORWARD_TOKEN ();
	  name = parse_opt_name_string (0);
	  val = ignoring ? val : build_chill_indirect_ref (val, name, 1);
	  continue;
	case LPRN:
	  /* The SEND buffer action syntax is ambiguous, at least when
	     parsed left-to-right.  In the example 'SEND foo(v) ...' the
	     phrase 'foo(v)' could be a buffer location procedure call
	     (which then must be followed by the value to send).
	     On the other hand, if 'foo' is a buffer, stop parsing
	     after 'foo', and let parse_send_action pick up '(v) as
	     the value ot send.

	     We handle the ambiguity for SEND signal action differently,
	     since we allow (as an extension) a signal to be used as
	     a "function" (see build_generalized_call). */
	  if (TREE_TYPE (val) != NULL_TREE
	      && CH_IS_BUFFER_MODE (TREE_TYPE (val)))
	    return val;
	  val = parse_call (val);
	  continue;
	case STRING:
	case BITSTRING:
	case SINGLECHAR:
	case NAME:
	  /* Handle string repetition. (See comment in parse_operand5.) */
	  args = parse_primval ();
	  val = ignoring ? val : build_generalized_call (val, args);
	  continue;
	default:
	  break;
	}
      break;
    }
  return val;
}

static tree
parse_operand6 ()
{
  if (check_token (RECEIVE))
    {
      tree location ATTRIBUTE_UNUSED = parse_primval ();
      sorry ("RECEIVE expression");
      return integer_one_node;
    }
  else if (check_token (ARROW))
    {
      tree location = parse_primval ();
      return ignoring ? location : build_chill_arrow_expr (location, 0);
    }
  else
    return parse_primval();
}

static tree
parse_operand5()
{
  enum tree_code op;
  /* We are supposed to be looking for a <string repetition operator>,
     but in general we can't distinguish that from a parenthesized
     expression.  This is especially difficult if we allow the
     string operand to be a constant expression (as requested by
     some users), and not just a string literal.
     Consider:  LPRN expr RPRN LPRN expr RPRN
     Is that a function call or string repetition?
     Instead, we handle string repetition in parse_primval,
     and build_generalized_call. */
  tree rarg;
  switch (PEEK_TOKEN())
    {
    case NOT:  op = BIT_NOT_EXPR; break;
    case SUB:  op = NEGATE_EXPR; break;
    default:
      op = NOP_EXPR;
    }
    if (op != NOP_EXPR)
      FORWARD_TOKEN();
    rarg = parse_operand6();
    return (op == NOP_EXPR || ignoring) ? rarg
      : build_chill_unary_op (op, rarg);
}

static tree
parse_operand4 ()
{
  tree larg = parse_operand5(), rarg;
  enum tree_code op;
  for (;;)
    {
      switch (PEEK_TOKEN())
	{
	case MUL:  op = MULT_EXPR; break;
	case DIV:  op = TRUNC_DIV_EXPR; break;
	case MOD:  op = FLOOR_MOD_EXPR; break;
	case REM:  op = TRUNC_MOD_EXPR; break;
	default:
	return larg;
	}
      FORWARD_TOKEN();
      rarg = parse_operand5();
      if (!ignoring)
	larg = build_chill_binary_op (op, larg, rarg);
    }
}

static tree
parse_operand3 ()
{
  tree larg = parse_operand4 (), rarg;
  enum tree_code op;
  for (;;)
    {
      switch (PEEK_TOKEN())
	{
	case PLUS:   op = PLUS_EXPR; break;
	case SUB:    op = MINUS_EXPR; break;
	case CONCAT: op = CONCAT_EXPR; break;
	default:
	return larg;
	}
      FORWARD_TOKEN();
      rarg = parse_operand4();
      if (!ignoring)
	larg = build_chill_binary_op (op, larg, rarg);
    }
}

static tree
parse_operand2 ()
{
  tree larg = parse_operand3 (), rarg;
  enum tree_code op;
  for (;;)
    {
      if (check_token (IN))
	{
	  rarg = parse_operand3();
	  if (! ignoring)
	    larg = build_chill_binary_op (SET_IN_EXPR, larg, rarg);
	}
      else
	{
	  switch (PEEK_TOKEN())
	    {
	    case GT:  op = GT_EXPR; break;
	    case GTE: op = GE_EXPR; break;
	    case LT:  op = LT_EXPR; break;
	    case LTE: op = LE_EXPR; break;
	    case EQL: op = EQ_EXPR; break;
	    case NE:  op = NE_EXPR; break;
	    default:
	      return larg;
	    }
	  FORWARD_TOKEN();
	  rarg = parse_operand3();
	  if (!ignoring)
	    larg = build_compare_expr (op, larg, rarg);
	}
    }
}

static tree
parse_operand1 ()
{
  tree larg = parse_operand2 (), rarg;
  enum tree_code op;
  for (;;)
    {
      switch (PEEK_TOKEN())
	{
	case AND:   op = BIT_AND_EXPR; break;
	case ANDIF: op = TRUTH_ANDIF_EXPR; break;
	default:
	  return larg;
	}
      FORWARD_TOKEN();
      rarg = parse_operand2();
      if (!ignoring)
	larg = build_chill_binary_op (op, larg, rarg);
    }
}

static tree
parse_operand0 ()
{
  tree larg = parse_operand1(), rarg;
  enum tree_code op;
  for (;;)
    {
      switch (PEEK_TOKEN())
	{
	case OR:  op = BIT_IOR_EXPR; break;
	case XOR:  op = BIT_XOR_EXPR; break;
	case ORIF:  op = TRUTH_ORIF_EXPR; break;
	default:
	  return larg;
	}
      FORWARD_TOKEN();
      rarg = parse_operand1();
      if (!ignoring)
	larg = build_chill_binary_op (op, larg, rarg);
    }
}

static tree
parse_expression ()
{
    return parse_operand0 ();
}

static tree
parse_case_expression ()
{
  tree selector_list;
  tree else_expr;
  tree case_expr;
  tree case_alt_list = NULL_TREE;

  require (CASE);
  selector_list = parse_expr_list ();
  selector_list = nreverse (selector_list);

  expect (OF, "missing 'OF'");
  while (PEEK_TOKEN () == LPRN)
    {
      tree label_spec = parse_case_label_specification (selector_list);
      tree sub_expr;
      expect (COLON, "missing ':' in value case alternative");
      sub_expr = parse_expression ();
      expect (SC, "missing ';'");
      if (! ignoring)
	case_alt_list = tree_cons (label_spec, sub_expr, case_alt_list);
    }
  if (check_token (ELSE))
    {
      else_expr = parse_expression ();
      if (check_token (SC) && pass == 1)
	warning("there should not be a ';' here"); 
    }
  else
    else_expr = NULL_TREE;
  expect (ESAC, "missing 'ESAC' in 'CASE' expression");

  if (ignoring)
    return integer_zero_node;

  /* If this is a multi dimension case, then transform it into an COND_EXPR
     here. This must be done before store_expr is called since it has some
     special handling for COND_EXPR expressions. */
  if (TREE_CHAIN (selector_list) != NULL_TREE)
    {
      case_alt_list = nreverse (case_alt_list);
      compute_else_ranges (selector_list, case_alt_list);
      case_expr =
	build_chill_multi_dimension_case_expr (selector_list, case_alt_list, else_expr);
    }
  else
    case_expr = build_chill_case_expr (selector_list, case_alt_list, else_expr);

  return case_expr;
}

static tree
parse_then_alternative ()
{
  expect (THEN, "missing 'THEN' in 'IF' expression");
  return parse_expression ();
}

static tree
parse_else_alternative ()
{
  if (check_token (ELSIF))
    return parse_if_expression_body ();
  else if (check_token (ELSE))
    return parse_expression ();
  error ("missing ELSE/ELSIF in IF expression");
  return error_mark_node;
}

/* Matches: <boolean expression> <then alternative> <else alternative> */

static tree
parse_if_expression_body ()
{
  tree bool_expr, then_expr, else_expr;
  bool_expr = parse_expression ();
  then_expr = parse_then_alternative ();
  else_expr = parse_else_alternative ();
  if (ignoring)
    return integer_zero_node;
  else
    return build_nt (COND_EXPR, bool_expr, then_expr, else_expr);
}

static tree
parse_if_expression ()
{
  tree expr;
  require (IF);
  expr = parse_if_expression_body ();
  expect (FI, "missing 'FI' at end of conditional expression");
  return expr;
}

/* An <untyped_expr> is a superset of <expr>.  It also includes
   <conditional expressions> and untyped <tuples>, whose types
   are not given by their constituents.  Hence, these are only
   allowed in certain contexts that expect a certain type.
   You should call convert() to fix up the <untyped_expr>. */

static tree
parse_untyped_expr ()
{
  tree val;
  switch (PEEK_TOKEN())
    {
    case IF:
      return parse_if_expression ();
    case CASE:
      return parse_case_expression ();
    case LPRN:
      switch (PEEK_TOKEN1())
	{
	case IF:
	case CASE:
	  if (pass == 1)
	    pedwarn ("conditional expression not allowed inside parentheses");
	  goto skip_lprn;
	case LPC:
	  if (pass == 1)
	    pedwarn ("mode-less tuple not allowed inside parentheses");
	skip_lprn:
	  FORWARD_TOKEN ();
	  val = parse_untyped_expr ();
	  expect (RPRN, "missing ')'");
	  return val;
	default: ;
	  /* fall through */
	}
    default:
      return parse_operand0 ();
    }
}

/* Matches:  <index mode> */

static tree
parse_index_mode ()
{
  /* This is another one that is nasty to parse!
   Let's feel our way ahead ... */
  tree lower, upper;
  if (PEEK_TOKEN () == NAME)
    {
      tree name = parse_name ();
      switch (PEEK_TOKEN ())
	{
	case COMMA:
	case RPRN:
	case SC: /* An error */
	  /* This can only (legally) be a discrete mode name. */
	  return name;
	case LPRN:
	  /* This could be named discrete range,
	     a cast, or some other expression (maybe). */
	  require (LPRN);
	  lower = parse_expression ();
	  if (check_token (COLON))
	    {
	      upper = parse_expression ();
	      expect (RPRN, "missing ')'");
	      /* Matched: <mode_name> '(' <expr> ':' <expr> ')' */
	      if (ignoring)
		return NULL_TREE;
	      else
		return build_chill_range_type (name, lower, upper);
	    }
	  /* Looks like a cast or procedure call or something.
	     Backup, and try again. */
	  pushback_token (EXPR, lower);
	  pushback_token (LPRN, NULL_TREE);
	  lower = parse_call (name);
	  goto parse_literal_range_colon;
	default:
	  /* This has to be the start of an expression. */
	  pushback_token (EXPR, name);
	  goto parse_literal_range;
	}
    }
  /* It's not a name.  But it could still be a discrete mode. */
  lower = parse_opt_mode ();
  if (lower)
    return lower;
 parse_literal_range:
  /* Nope, it's a discrete literal range. */
  lower = parse_expression ();
 parse_literal_range_colon:
  expect (COLON, "expected ':' here");
  
  upper = parse_expression ();
  return ignoring ? NULL_TREE
    : build_chill_range_type (NULL_TREE, lower, upper);
}

static tree
parse_set_mode ()
{
  int  set_name_cnt = 0;          /* count of named set elements */
  int  set_is_numbered = 0;     /* TRUE if set elements have explicit values */
  int  set_is_not_numbered = 0;
  tree list = NULL_TREE;
  tree mode = ignoring ? void_type_node : start_enum (NULL_TREE);
  require (SET);
  expect (LPRN, "missing left parenthesis after SET");
  for (;;)
    {
      tree name, value = NULL_TREE;
      if (check_token (MUL))
	name = NULL_TREE;
      else
	{
	  name = parse_defining_occurrence ();
	  if (check_token (EQL))
	    {
	      value = parse_expression ();
	      set_is_numbered = 1;
	    }
	  else
	    set_is_not_numbered = 1;
	  set_name_cnt++;
	}
      name = build_enumerator (name, value);
      if (pass == 1)
	list = chainon (name, list);
      if (! check_token (COMMA))
	break;
    }
  expect (RPRN, "missing right parenthesis after SET");
  if (!ignoring)
    {
      if (set_is_numbered && set_is_not_numbered)
	/* Z.200 doesn't allow mixed numbered and unnumbered set elements,
	   but we can do it. Print a warning */
	pedwarn ("mixed numbered and unnumbered set elements is not standard");
      mode = finish_enum (mode, list); 
      if (set_name_cnt == 0)
	error ("SET mode must define at least one named value");
      CH_ENUM_IS_NUMBERED(mode) = set_is_numbered ? 1 : 0;
    }
  return mode;
}

/* parse layout POS:
   returns a tree with following layout

                treelist
       pupose=treelist  value=NULL_TREE (to indicate POS)
     pupose=word  value=treelist | NULL_TREE
           pupose=startbit  value=treelist | NULL_TREE
                      purpose=                      value=
               integer_zero | integer_one    length | endbit
*/
static tree
parse_pos ()
{
  tree word;
  tree startbit = NULL_TREE, endbit = NULL_TREE;
  tree what = NULL_TREE;
  
  require (LPRN);
  word = parse_untyped_expr ();
  if (check_token (COMMA))
    {
      startbit = parse_untyped_expr ();
      if (check_token (COMMA))
	{
	  what = integer_zero_node;
	  endbit = parse_untyped_expr ();
	}
      else if (check_token (COLON))
	{
	  what = integer_one_node;
	  endbit = parse_untyped_expr ();
	}
    }
  require (RPRN);
  
  /* build the tree as described above */
  if (what != NULL_TREE)
    what = tree_cons (what, endbit, NULL_TREE);
  if (startbit != NULL_TREE)
    startbit = tree_cons (startbit, what, NULL_TREE);
  endbit = tree_cons (word, startbit, NULL_TREE);
  return tree_cons (endbit, NULL_TREE, NULL_TREE);
}

/* parse layout STEP
   returns a tree with the following layout

                treelist
     pupose=NULL_TREE value=treelist (to indicate STEP)
         pupose=POS(see baove)  value=stepsize | NULL_TREE
*/
static tree
parse_step ()
{
  tree pos;
  tree stepsize = NULL_TREE;
  
  require (LPRN);
  require (POS);
  pos = parse_pos ();
  if (check_token (COMMA))
    stepsize = parse_untyped_expr ();
  require (RPRN);
  TREE_VALUE (pos) = stepsize;
  return tree_cons (NULL_TREE, pos, NULL_TREE);
}

/* returns layout for fields or array elements.
   NULL_TREE            no layout specified
   integer_one_node     PACK specified
   integer_zero_node    NOPACK specified
   tree_list PURPOSE    POS
   tree_list VALUE      STEP
*/
static tree
parse_opt_layout (in)
     int in;     /* 0 ... parse structure, 1 ... parse array */
{
  tree val = NULL_TREE;

  if (check_token (PACK))
    {
      return integer_one_node;
    }
  else if (check_token (NOPACK))
    {
      return integer_zero_node;
    }
  else if (check_token (POS))
    {
      val = parse_pos ();
      if (in == 1 && pass == 1)
	{
	  error ("POS not allowed for ARRAY");
	  val = NULL_TREE;
	}
      return val;
    }
  else if (check_token (STEP))
    {
      val = parse_step ();
      if (in == 0 && pass == 1)
	{
	  error ("STEP not allowed in field definition");
	  val = NULL_TREE;
	}
      return val;
    }
  else
    return NULL_TREE;
}

static tree
parse_field_name_list ()
{
  tree chain = NULL_TREE;
  tree name = parse_defining_occurrence ();
  if (name == NULL_TREE)
    {
      error("missing field name");
      return NULL_TREE;
    }
  chain = build_tree_list (NULL_TREE, name);
  while (check_token (COMMA))
    {
      name = parse_defining_occurrence ();
      if (name == NULL)
	{
	  error ("bad field name following ','");
	  break;
	}
      if (! ignoring)
	chain = tree_cons (NULL_TREE, name, chain);
    }
  return chain;
}

/* Matches: <fixed field> or <variant field>, i.e.:
   <field name defining occurrence list> <mode> [ <field layout> ].
   Returns:  A chain of FIELD_DECLs.
   NULL_TREE is returned if ignoring is true or an error is seen. */

static tree
parse_fixed_field ()
{
  tree field_names = parse_field_name_list ();
  tree mode = parse_mode ();
  tree layout = parse_opt_layout (0);
  return ignoring ? NULL_TREE
    : grok_chill_fixedfields (field_names, mode, layout);
}


/* Matches: [ <variant field> { "," <variant field> }* ]
   Returns:  A chain of FIELD_DECLs.
   NULL_TREE is returned if ignoring is true or an error is seen. */

static tree
parse_variant_field_list ()
{
  tree fields = NULL_TREE;
  if (PEEK_TOKEN () != NAME)
    return NULL_TREE;
  for (;;)
    {
      fields = chainon (fields, parse_fixed_field ());
      if (PEEK_TOKEN () != COMMA || PEEK_TOKEN1 () != NAME)
	break;
      require (COMMA);
    }
  return fields;
}

/* Matches: <variant alternative>
   Returns a TREE_LIST node, whose TREE_PURPOSE (if non-NULL) is the label,
   and whose TREE_VALUE is the list of FIELD_DECLs. */

static tree
parse_variant_alternative ()
{
  tree labels;

  if (PEEK_TOKEN () == LPRN)
    labels = parse_case_label_specification (NULL_TREE);
  else
    labels = NULL_TREE;
  if (! check_token (COLON))
    {
      error ("expected ':' in structure variant alternative");
      return NULL_TREE;
    }

  /* We now read a list a variant fields, until we come to the end
     of the variant alternative.  But since both variant fields
     *and* variant alternatives are separated by COMMAs,
     we will have to look ahead to distinguish the start of a variant
     field from the start of a new variant alternative.
     We use the fact that a variant alternative must start with
     either a LPRN or a COLON, while a variant field must start with a NAME.
     This look-ahead is handled by parse_simple_fields. */
  return build_tree_list (labels, parse_variant_field_list ());
}

/* Parse <field> (which is <fixed field> or <alternative field>).
   Returns:  A chain of FIELD_DECLs (or NULL_TREE on error or if ignoring). */

static tree
parse_field ()
{
  if (check_token (CASE))
    {
      tree tag_list = NULL_TREE, variants, opt_variant_else;
      if (PEEK_TOKEN () == NAME)
	{
	  tag_list = nreverse (parse_field_name_list ());
	  if (pass == 1)
	    tag_list = lookup_tag_fields (tag_list, current_fieldlist);
	}
      expect (OF, "missing 'OF' in alternative structure field");

      variants = parse_variant_alternative ();
      while (check_token (COMMA))
	variants = chainon (parse_variant_alternative (), variants);
      variants = nreverse (variants);

      if (check_token (ELSE))
	opt_variant_else = parse_variant_field_list ();
      else
	opt_variant_else = NULL_TREE;
      expect (ESAC, "missing 'ESAC' following alternative structure field");
      if (ignoring)
	return NULL_TREE;
      return grok_chill_variantdefs (tag_list, variants, opt_variant_else);
    }
  else if (PEEK_TOKEN () == NAME)
    return parse_fixed_field ();
  else
    {
      if (pass == 1)
	error ("missing field");
      return NULL_TREE;
    }
}

static tree
parse_structure_mode ()
{
  tree save_fieldlist = current_fieldlist;
  tree fields;
  require (STRUCT);
  expect (LPRN, "expected '(' after STRUCT");
  current_fieldlist = fields = parse_field ();
  while (check_token (COMMA))
    fields = chainon (fields, parse_field ());
  expect (RPRN, "expected ')' after STRUCT");
  current_fieldlist = save_fieldlist;
  return ignoring ? void_type_node : build_chill_struct_type (fields);
}

static tree
parse_opt_queue_size ()
{
  if (check_token (LPRN))
    {
      tree size = parse_expression ();
      expect (RPRN, "missing ')'");
      return size;
    }
  else
    return NULL_TREE;
}

static tree
parse_procedure_mode ()
{
  tree param_types = NULL_TREE, result_spec, except_list, recursive;
  require (PROC);
  expect (LPRN, "missing '(' after PROC");
  if (! check_token (RPRN))
    {
      for (;;)
	{
	  tree pmode = parse_mode ();
	  tree paramattr = parse_param_attr ();
	  if (! ignoring)
	    {
	      pmode = get_type_of (pmode);
	      param_types = tree_cons (paramattr, pmode, param_types);
	    }
	  if (! check_token (COMMA))
	    break;
	}
      expect (RPRN, "missing ')' after PROC");
    }
  result_spec = parse_opt_result_spec ();
  except_list = parse_opt_except ();
  recursive = parse_opt_recursive ();
  if (ignoring)
    return void_type_node;
  return build_chill_pointer_type (build_chill_function_type
				   (result_spec, nreverse (param_types),
				    except_list, recursive));
}

/* Matches: <mode>
   A NAME will be assumed to be a <mode name>, and thus a <mode>.
   Returns NULL_TREE if no mode is seen.
   (If ignoring is true, the return value may be an arbitrary tree node,
   but will be non-NULL if something that could be a mode is seen.) */

static tree
parse_opt_mode ()
{
  switch (PEEK_TOKEN ())
    {
    case ACCESS:
      {
	tree index_mode, record_mode;
	int dynamic = 0;
	require (ACCESS);
	if (check_token (LPRN))
	  {
	    index_mode = parse_index_mode ();
	    expect (RPRN, "mssing ')'");
	  }
	else
	  index_mode = NULL_TREE;
	record_mode = parse_opt_mode ();
	if (record_mode)
	  dynamic = check_token (DYNAMIC);
	return ignoring ? void_type_node
	                : build_access_mode (index_mode, record_mode, dynamic);
      }
    case ARRAY:
      {
	tree index_list = NULL_TREE, base_mode;
	int varying;
	int num_index_modes = 0;
	int i;
	tree layouts = NULL_TREE;
	FORWARD_TOKEN ();
	expect (LPRN, "missing '(' after ARRAY");
	for (;;)
	  {
	    tree index = parse_index_mode ();
	    num_index_modes++;
	    if (!ignoring)
	      index_list = tree_cons (NULL_TREE, index, index_list);
	    if (! check_token (COMMA))
	      break;
	  }
	expect (RPRN, "missing ')' after ARRAY");
	varying = check_token (VARYING);
	base_mode = parse_mode ();
	/* Allow a layout specification for each index mode */
	for (i = 0; i < num_index_modes; ++i)
	  {
	  tree new_layout = parse_opt_layout (1);
	  if (new_layout == NULL_TREE)
	    break;
	  if (!ignoring)
	    layouts = tree_cons (NULL_TREE, new_layout, layouts);
	  }
	if (ignoring)
	  return base_mode;
	return build_chill_array_type (get_type_of (base_mode),
				       index_list, varying, layouts);
      }
    case ASSOCIATION:
      require (ASSOCIATION);
      return association_type_node;
    case BIN:
      { tree length;
	FORWARD_TOKEN();
	expect (LPRN, "missing left parenthesis after BIN");
	length = parse_expression ();
	expect (RPRN, "missing right parenthesis after BIN");
	return ignoring ? void_type_node :  build_chill_bin_type (length);
      } 
    case BOOLS:
      {
	tree length;
	FORWARD_TOKEN ();
	expect (LPRN, "missing '(' after BOOLS");
	length = parse_expression ();
	expect (RPRN, "missing ')' after BOOLS");
	if (check_token (VARYING))
	  error ("VARYING bit-strings not implemented");
	return ignoring ? void_type_node : build_bitstring_type (length);
      }
    case BUFFER:
      {
	tree qsize, element_mode;
	require (BUFFER);
	qsize = parse_opt_queue_size ();
	element_mode = parse_mode ();
	return ignoring ? element_mode
	  : build_buffer_type (element_mode, qsize);
      }
    case CHARS:
      {
	tree length;
	int varying;
	tree type;
	FORWARD_TOKEN ();
	expect (LPRN, "missing '(' after CHARS");
	length = parse_expression ();
	expect (RPRN, "missing ')' after CHARS");
	varying = check_token (VARYING);
	if (ignoring)
	  return void_type_node;
	type = build_string_type (char_type_node, length);
	if (varying)
	  type = build_varying_struct (type);
	return type;
      }
    case EVENT:
      {
	tree qsize;
	require (EVENT);
	qsize = parse_opt_queue_size ();
	return ignoring ? void_type_node : build_event_type (qsize);
      }
    case NAME:
      {
	tree mode = get_type_of (parse_name ());
	if (check_token (LPRN))
	  {
	    tree min_value = parse_expression ();
	    if (check_token (COLON))
	      {
		tree max_value = parse_expression ();
		expect (RPRN, "syntax error - expected ')'");
		/* Matched: <mode_name> '(' <expr> ':' <expr> ')' */
		if (ignoring)
		  return mode;
		else
		  return build_chill_range_type (mode, min_value, max_value);
	      }
	    if (check_token (RPRN))
	      {
		int varying = check_token (VARYING);
		if (! ignoring)
		  {
		    if (mode == char_type_node || varying)
		      {
			if (mode != char_type_node
			    && mode != ridpointers[(int) RID_CHAR])
			  error ("strings must be composed of chars");
			mode = build_string_type (char_type_node, min_value);
			if (varying)
			  mode  = build_varying_struct (mode);
		      }
		    else
		      {
			/* Parameterized mode,
			   or old-fashioned CHAR(N) string declaration.. */
			tree pmode = make_node (LANG_TYPE);
			TREE_TYPE (pmode) = mode;
			TYPE_DOMAIN (pmode) = min_value;
			mode = pmode;
		      }
		  }
	      }
	  }
	return mode;
      }
    case POWERSET:
      { tree mode;
	FORWARD_TOKEN ();
	mode = parse_mode ();
	 if (ignoring || TREE_CODE (mode) == ERROR_MARK)
	   return mode;
	return build_powerset_type (get_type_of (mode)); 
      }
    case PROC:
      return parse_procedure_mode ();
    case RANGE:
      { tree low, high;
	FORWARD_TOKEN();
	expect (LPRN, "missing left parenthesis after RANGE");
	low = parse_expression ();
	expect (COLON, "missing colon");
	high = parse_expression ();
	expect (RPRN, "missing right parenthesis after RANGE");
	return ignoring ? void_type_node
	  :  build_chill_range_type (NULL_TREE, low, high);
      }
    case READ:
	FORWARD_TOKEN ();
	{
	  tree mode2 = get_type_of (parse_mode ());
	  if (ignoring || TREE_CODE (mode2) == ERROR_MARK)
	    return mode2;
	  if (mode2
	      && TREE_CODE_CLASS (TREE_CODE (mode2)) == 'd'
	      && CH_IS_BUFFER_MODE (mode2))
	    {
	      error ("BUFFER modes may not be readonly");
	      return mode2;
	    }
	  if (mode2
	      && TREE_CODE_CLASS (TREE_CODE (mode2)) == 'd'
	      && CH_IS_EVENT_MODE (mode2))
	    {
	      error ("EVENT modes may not be readonly");
	      return mode2;
	    }
	  return build_readonly_type (mode2);

      }
    case REF:
      { tree mode;
	FORWARD_TOKEN ();
	mode = parse_mode ();
	 if (ignoring)
	   return mode;
	mode = get_type_of (mode);
	return (TREE_CODE (mode) == ERROR_MARK) ? mode
	  : build_chill_pointer_type (mode); 
      }
    case SET:
      return parse_set_mode ();
    case SIGNAL:
      if (pedantic)
	error ("SIGNAL is not a valid mode");
      return generic_signal_type_node; 
    case STRUCT:
      return parse_structure_mode ();
    case TEXT:
      {
	tree length, index_mode;
	int dynamic;
	require (TEXT);
	expect (LPRN, "missing '('");
	length = parse_expression ();
	expect (RPRN, "missing ')'");
	/* FIXME:  This should actually look for an optional index_mode,
	   but that is tricky to do. */
	index_mode = parse_opt_mode ();
	dynamic = check_token (DYNAMIC);
	return ignoring ? void_type_node
	                : build_text_mode (length, index_mode, dynamic);
      }
    case USAGE:
      require (USAGE);
      return usage_type_node;
    case WHERE:
      require (WHERE);
      return where_type_node;
    default:
      return NULL_TREE; 
    }
}

static tree
parse_mode ()
{
 tree mode = parse_opt_mode ();
 if (mode == NULL_TREE)
   {
     if (pass == 1)
       error ("syntax error - missing mode");
     mode = error_mark_node;
   }
 return mode;
}

static void
parse_program()
{
  /* Initialize global variables for current pass. */
  int i;
  expand_exit_needed = 0;
  label = NULL_TREE;             /* for statement labels */
  current_module = NULL;
  current_function_decl = NULL_TREE;
  in_pseudo_module = 0;
  
  for (i = 0; i <= MAX_LOOK_AHEAD; i++)
    terminal_buffer[i] = TOKEN_NOT_READ;

#if 0
  /* skip some junk */
  while (PEEK_TOKEN() == HEADEREL)
    FORWARD_TOKEN();
#endif

  start_outer_function ();

  for (;;)
    {
      tree label = parse_optlabel ();
      if (PEEK_TOKEN() == MODULE || PEEK_TOKEN() == REGION)
	parse_modulion (label);
      else if (PEEK_TOKEN() == SPEC)
	parse_spec_module (label);
      else break;
    }

  finish_outer_function ();
}

static void
parse_pass_1_2()
{
  parse_program();
  if (PEEK_TOKEN() != END_PASS_1)
    {
      error ("syntax error - expected a module or end of file");
      serious_errors++;
    }
  chill_finish_compile ();
  if (serious_errors)
    exit (FATAL_EXIT_CODE);
  switch_to_pass_2 ();
  ch_parse_init ();
  except_init_pass_2 ();
  ignoring = 0;
  parse_program();
  chill_finish_compile ();
}

int yyparse ()
{
  parse_pass_1_2 ();
  return 0;
}

/*
 * We've had an error.  Move the compiler's state back to
 * the global binding level.  This prevents the loop in
 * compile_file in toplev.c from looping forever, since the 
 * CHILL poplevel() has *no* effect on the value returned by 
 * global_bindings_p().
 */
void
to_global_binding_level ()
{
  while (! global_bindings_p ())
    current_function_decl = DECL_CONTEXT (current_function_decl);
  serious_errors++;
}

#if 1
int yydebug;
/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */
void
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}
#endif
