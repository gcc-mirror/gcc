/* YACC parser for C++ syntax.
   Copyright (C) 1988, 1989, 1993 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Note: Bison automatically applies a default action of "$$ = $1" for
   all derivations; this is applied before the explicit action, if one
   is given.  Keep this in mind when reading the actions.  */

/* Also note: this version contains experimental exception
   handling features.  They could break, change, disappear,
   or otherwise exhibit volatile behavior.  Don't depend on
   me (Michael Tiemann) to protect you from any negative impact
   this may have on your professional, personal, or spiritual life.

   NEWS FLASH:  This version now supports the exception handling
   syntax of Stroustrup's 2nd edition, if -fansi-exceptions is given.
   THIS IS WORK IN PROGRESS!!!  The type of the 'throw' and the
   'catch' much match EXACTLY (no inheritance support or coercions).
   Also, throw-specifications of functions don't work.
   Destructors aren't called correctly.  Etc, etc.  --Per Bothner.
  */

%{
/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

#include "config.h"

#include <stdio.h>
#include <errno.h>

#include "tree.h"
#include "input.h"
#include "flags.h"
#include "lex.h"
#include "cp-tree.h"

/* Since parsers are distinct for each language, put the language string
   definition here.  (fnf) */
char *language_string = "GNU C++";

extern tree void_list_node;
extern struct obstack permanent_obstack;

#ifndef errno
extern int errno;
#endif

extern int end_of_file;
extern int current_class_depth;

void yyerror ();

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

#define OP0(NODE) (TREE_OPERAND (NODE, 0))
#define OP1(NODE) (TREE_OPERAND (NODE, 1))

/* Contains the statement keyword (if/while/do) to include in an
   error message if the user supplies an empty conditional expression.  */
static char *cond_stmt_keyword;

/* Nonzero if we have an `extern "C"' acting as an extern specifier.  */
int have_extern_spec;
int used_extern_spec;

void yyhook ();

/* Cons up an empty parameter list.  */
#ifdef __GNUC__
__inline
#endif
static tree
empty_parms ()
{
  tree parms;

  if (strict_prototype)
    parms = void_list_node;
  else
    parms = NULL_TREE;
  return parms;
}
%}

%start program

%union {long itype; tree ttype; char *strtype; enum tree_code code; }

/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC

/* Reserved words that qualify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPE_QUAL

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else. */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM_KEYWORD GCC_ASM_KEYWORD TYPEOF ALIGNOF
%token HEADOF CLASSOF SIGOF
%token ATTRIBUTE EXTENSION LABEL

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <itype> VISSPEC
%token DELETE NEW OVERLOAD THIS OPERATOR CXX_TRUE CXX_FALSE
%token LEFT_RIGHT TEMPLATE
%token TYPEID DYNAMIC_CAST STATIC_CAST REINTERPRET_CAST CONST_CAST
%token <itype> SCOPE

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%left EMPTY			/* used to resolve s/r with epsilon */

%left error

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

%left IDENTIFIER TYPENAME PTYPENAME SCSPEC TYPESPEC TYPE_QUAL ENUM AGGR ELLIPSIS TYPEOF SIGOF OPERATOR

%left '{' ',' ';'

%right <code> ASSIGN '='
%right <code> '?' ':'
%left <code> OROR
%left <code> ANDAND
%left <code> '|'
%left <code> '^'
%left <code> '&'
%left <code> MIN_MAX
%left <code> EQCOMPARE
%left <code> ARITHCOMPARE '<' '>'
%left <code> LSHIFT RSHIFT
%left <code> '+' '-'
%left <code> '*' '/' '%'
%left <code> POINTSAT_STAR DOT_STAR
%right <code> UNARY PLUSPLUS MINUSMINUS '~'
%left HYPERUNARY
%left <ttype> PAREN_STAR_PAREN LEFT_RIGHT
%left <code> POINTSAT '.' '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE TRY CATCH THROW

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> paren_expr_or_null nontrivial_exprlist
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> typed_declspecs reserved_declspecs boolean.literal
%type <ttype> typed_typespecs reserved_typespecquals
%type <ttype> declmods typespec typespecqual_reserved
%type <ttype> SCSPEC TYPESPEC TYPE_QUAL nonempty_type_quals maybe_type_qual
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attributes attribute attribute_list attrib
%type <ttype> any_word

%type <ttype> compstmt implicitly_scoped_stmt

%type <ttype> declarator notype_declarator after_type_declarator
%type <ttype> direct_notype_declarator direct_after_type_declarator

%type <ttype> structsp opt.component_decl_list component_decl_list
%type <ttype> component_decl components component_declarator
%type <ttype> notype_components notype_component_declarator
%type <ttype> after_type_component_declarator after_type_component_declarator0
%type <ttype> notype_component_declarator0 component_decl_1
%type <ttype> enumlist enumerator
%type <ttype> type_id absdcl type_quals
%type <ttype> direct_abstract_declarator conversion_declarator
%type <ttype> new_type_id new_declarator direct_new_declarator
%type <ttype> xexpr parmlist parms parm bad_parm full_parm
%type <ttype> identifiers_or_typenames
%type <ttype> fcast_or_absdcl regcast_or_absdcl sub_cast_expr
%type <ttype> expr_or_declarator complex_notype_declarator
%type <ttype> notype_unqualified_id unqualified_id qualified_id
%type <ttype> overqualified_id notype_qualified_id
%type <ttype> complex_direct_notype_declarator functional_cast
%type <ttype> named_parm complex_parmlist typed_declspecs1 parms_comma

/* C++ extensions */
%token <ttype> TYPENAME_ELLIPSIS PTYPENAME
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL
%type <ttype> fn.def1 /* Not really! */
%type <ttype> fn.def2 return_id
%type <ttype> named_class_head named_class_head_sans_basetype
%type <ttype> unnamed_class_head
%type <ttype> class_head base_class_list
%type <itype> base_class_access_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> maybe_raises ansi_raise_identifier ansi_raise_identifiers
%type <ttype> component_declarator0
%type <ttype> forhead.1 operator_name
%type <ttype> object aggr
%type <itype> new delete
/* %type <ttype> primary_no_id */
%type <ttype> nonmomentary_expr
%type <itype> forhead.2 initdcl0 notype_initdcl0 member_init_list
%type <ttype> template_header template_parm_list template_parm
%type <ttype> template_type_parm
%type <ttype> template_type template_arg_list template_arg
%type <ttype> template_instantiation template_type_name tmpl.2
%type <ttype> template_instantiate_once template_instantiate_some
%type <itype> fn_tmpl_end
/* %type <itype> try_for_typename */
%type <ttype> condition xcond paren_cond_or_null
%type <ttype> type_name nested_name_specifier nested_type ptr_to_mem
%type <ttype> qualified_type_name complete_type_name notype_identifier
%type <ttype> complex_type_name nested_name_specifier_1
%type <itype> nomods_initdecls nomods_initdcl0
%type <ttype> new_initializer new_placement specialization type_specifier_seq

/* in order to recognize aggr tags as defining and thus shadowing. */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> named_class_head_sans_basetype_defn 
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN

%type <strtype> .pushlevel

/* spew.c depends on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT

%{
/* List of types and structure classes of the current declaration.  */
static tree current_declspecs;

/* When defining an aggregate, this is the most recent one being defined.  */
static tree current_aggr;

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
extern void yyprint ();
extern tree combine_strings		PROTO((tree));
%}

%%
program: /* empty */
	| extdefs
		{
		  /* In case there were missing closebraces,
		     get us back to the global binding level.  */
		  while (! global_bindings_p ())
		    poplevel (0, 0, 0);
		  finish_file ();
		}
	;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	  { $<ttype>$ = NULL_TREE; } lang_extdef
		{ $<ttype>$ = NULL_TREE; }
	| extdefs lang_extdef
		{ $<ttype>$ = NULL_TREE; }
	;

.hush_warning:
		{ have_extern_spec = 1;
		  used_extern_spec = 0;
		  $<ttype>$ = NULL_TREE; }
	;
.warning_ok:
		{ have_extern_spec = 0; }
	;

asm_keyword:
	  ASM_KEYWORD
	| GCC_ASM_KEYWORD
	;

lang_extdef:
	  { if (pending_lang_change) do_pending_lang_change(); }
	  extdef
	  { if (! global_bindings_p () && ! pseudo_global_level_p())
	      pop_everything (); }
	;

extdef:
	  fndef
		{ if (pending_inlines) do_pending_inlines (); }
	| datadef
		{ if (pending_inlines) do_pending_inlines (); }
	| template_def
		{ if (pending_inlines) do_pending_inlines (); }
	| overloaddef
	| asm_keyword '(' string ')' ';'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3);
		  assemble_asm ($3); }
	| extern_lang_string '{' extdefs '}'
		{ pop_lang_context (); }
	| extern_lang_string '{' '}'
		{ pop_lang_context (); }
	| extern_lang_string .hush_warning fndef .warning_ok
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	| extern_lang_string .hush_warning datadef .warning_ok
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	;

extern_lang_string:
	  EXTERN_LANG_STRING
		{ push_lang_context ($1); }
	;

template_header:
	  TEMPLATE '<'
		{ begin_template_parm_list (); }
	  template_parm_list '>'
		{ $$ = end_template_parm_list ($4); }
	;

template_parm_list:
	  template_parm
		{ $$ = process_template_parm (NULL_TREE, $1); }
	| template_parm_list ',' template_parm
		{ $$ = process_template_parm ($1, $3); }
	;

template_type_parm:
	  aggr
		{ 
		  $$ = build_tree_list ($1, NULL_TREE);
		 ttpa:
		  if (TREE_PURPOSE ($$) == signature_type_node)
		    sorry ("signature as template type parameter");
		  else if (TREE_PURPOSE ($$) != class_type_node)
		    pedwarn ("template type parameters must use the keyword `class'");
		}
	| aggr identifier
		{ $$ = build_tree_list ($1, $2); goto ttpa; }
	;

template_parm:
	/* The following rules introduce a new reduce/reduce
	   conflict on the ',' and '>' input tokens: they are valid
	   prefixes for a `structsp', which means they could match a
	   nameless parameter.  See 14.6, paragraph 3.
	   By putting them before the `parm' rule, we get
	   their match before considering them nameless parameter
	   declarations.  */
	  template_type_parm
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| template_type_parm '=' typespec
		{ $$ = build_tree_list ($3, $$); }
	| full_parm
	;

overloaddef:
	  OVERLOAD ov_identifiers ';'
		{ warning ("use of `overload' is an anachronism"); }
	;

ov_identifiers: IDENTIFIER
		{ declare_overloaded ($1); }
	| ov_identifiers ',' IDENTIFIER
		{ declare_overloaded ($3); }
	;
	  
template_def:
	/* Class template declarations go here; they aren't normal class
	   declarations, because we can't process the bodies yet.  */
	  template_header named_class_head_sans_basetype '{'
		{ yychar = '{'; goto template1; }
	 ';'
	| template_header named_class_head_sans_basetype_defn '{'
		{ yychar = '{'; goto template1; }
	 ';'
	| template_header named_class_head_sans_basetype ':'
		{ yychar = ':'; goto template1; }
	 ';'
	| template_header named_class_head_sans_basetype_defn ':'
		{
		  yychar = ':';
		template1:
		  if (current_aggr == exception_type_node)
		    error ("template type must define an aggregate or union");
		  else if (current_aggr == signature_type_node)
		    sorry ("template type defining a signature");
		  /* Maybe pedantic warning for union?
		     How about an enum? :-)  */
		  end_template_decl ($1, $2, current_aggr, 1);
		  reinit_parse_for_template (yychar, $1, $2);
		  yychar = YYEMPTY;
		}
	  ';'
	| template_header named_class_head_sans_basetype ';'
		{
		  end_template_decl ($1, $2, current_aggr, 0);
		  /* declare $2 as template name with $1 parm list */
		}
	| template_header named_class_head_sans_basetype_defn ';'
		{
		  end_template_decl ($1, $2, current_aggr, 0);
		  /* declare $2 as template name with $1 parm list */
		}
	| template_header /* notype_initdcl0 ';' */
	  notype_declarator maybe_raises maybeasm maybe_attribute
	  fn_tmpl_end
		{
		  tree d;
		  int momentary;
		  int def = ($6 != ';');
		  momentary = suspend_momentary ();
		  d = start_decl ($<ttype>2, /*current_declspecs*/NULL_TREE, 0,
				  $3);
		  cplus_decl_attributes (d, $5);
		  finish_decl (d, NULL_TREE, $4, 0);
		  end_template_decl ($1, d, 0, def);
		  if (def)
		    reinit_parse_for_template ((int) $6, $1, d);
		  resume_momentary (momentary);
		}
	| template_header typed_declspecs /*initdcl0*/
	  declarator maybe_raises maybeasm maybe_attribute
	  fn_tmpl_end
		{
		  tree d;
		  int momentary;
		  int def = ($7 != ';');

		  current_declspecs = $2;
		  momentary = suspend_momentary ();
		  d = start_decl ($<ttype>3, current_declspecs,
				  0, $<ttype>4);
		  cplus_decl_attributes (d, $6);
		  finish_decl (d, NULL_TREE, $5, 0);
		  end_template_decl ($1, d, 0, def);
		  if (def)
		    {
		      reinit_parse_for_template ((int) $7, $1, d);
		      yychar = YYEMPTY;
		    }
		  note_list_got_semicolon ($<ttype>2);
		  resume_momentary (momentary);
		}
	| template_header declmods notype_declarator fn_tmpl_end
		{
		  int def = ($4 != ';');
		  tree d = start_decl ($<ttype>3, $<ttype>2, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE, 0);
		  end_template_decl ($1, d, 0, def);
		  if (def)
		    reinit_parse_for_template ((int) $4, $1, d);
		}
	/* Try to recover from syntax errors in templates.  */
	| template_header error '}'	{ end_template_decl ($1, 0, 0, 0); }
	| template_header error ';'	{ end_template_decl ($1, 0, 0, 0); }
	;

fn_tmpl_end: '{'		{ $$ = '{'; }
	| ':'			{ $$ = ':'; }
	| ';'			{ $$ = ';'; }
	| '='			{ $$ = '='; }
	| RETURN		{ $$ = RETURN; }
	;

datadef:
	  nomods_initdecls ';'
		{}
	| declmods notype_initdecls ';'
		{}
	/* Normal case to make fast: "const i;".  */
	| declmods notype_declarator ';'
		{ tree d;
		  d = start_decl ($<ttype>2, $<ttype>$, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE, 0);
		}
	| typed_declspecs initdecls ';'
		{
		  note_list_got_semicolon ($<ttype>$);
		}
	/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
		{ tree d;
		  d = start_decl ($<ttype>2, $<ttype>$, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE, 0);
		  note_list_got_semicolon ($<ttype>$);
		}
        | declmods ';'
	  { pedwarn ("empty declaration"); }
	| explicit_instantiation ';'
	| typed_declspecs ';'
	  {
	    tree t = $<ttype>$;
	    shadow_tag (t);
	    if (TREE_CODE (t) == TREE_LIST
		&& TREE_PURPOSE (t) == NULL_TREE)
	      {
		t = TREE_VALUE (t);
		if (IS_AGGR_TYPE (t)
		    && IDENTIFIER_TEMPLATE (TYPE_IDENTIFIER (t)))
		  {
		    if (CLASSTYPE_USE_TEMPLATE (t) == 0)
		      SET_CLASSTYPE_TEMPLATE_SPECIALIZATION (t);
		    else if (CLASSTYPE_TEMPLATE_INSTANTIATION (t))
		      error ("override declaration for already-expanded template");
		  }
	      }
	    note_list_got_semicolon ($<ttype>$);
	  }
	| error ';'
	| error '}'
	| ';'
	;

fndef:
	  fn.def1 base_init compstmt_or_error
		{
		  finish_function (lineno, 1);
		  /* finish_function performs these three statements:

		     expand_end_bindings (getdecls (), 1, 0);
		     poplevel (1, 1, 0);

		     expand_end_bindings (0, 0, 0);
		     poplevel (0, 0, 1);
		     */
		  if ($<ttype>$) process_next_inline ($<ttype>$);
		}
	| fn.def1 return_init base_init compstmt_or_error
		{
		  finish_function (lineno, 1);
		  /* finish_function performs these three statements:

		     expand_end_bindings (getdecls (), 1, 0);
		     poplevel (1, 1, 0);

		     expand_end_bindings (0, 0, 0);
		     poplevel (0, 0, 1);
		     */
		  if ($<ttype>$) process_next_inline ($<ttype>$);
		}
	| fn.def1 nodecls compstmt_or_error
		{ finish_function (lineno, 0);
		  if ($<ttype>$) process_next_inline ($<ttype>$); }
	| fn.def1 return_init ';' nodecls compstmt_or_error
		{ finish_function (lineno, 0);
		  if ($<ttype>$) process_next_inline ($<ttype>$); }
	| fn.def1 return_init nodecls compstmt_or_error
		{ finish_function (lineno, 0);
		  if ($<ttype>$) process_next_inline ($<ttype>$); }
	| typed_declspecs declarator error
		{}
	| declmods notype_declarator error
		{}
	| notype_declarator error
		{}
	;

fn.def1:
	  typed_declspecs declarator maybe_raises
		{ if (! start_function ($$, $2, $3, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| declmods notype_declarator maybe_raises
		{ if (! start_function ($$, $2, $3, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| notype_declarator maybe_raises
		{ if (! start_function (NULL_TREE, $$, $2, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| PRE_PARSED_FUNCTION_DECL
		{ start_function (NULL_TREE, TREE_VALUE ($$), NULL_TREE, 1);
		  reinit_parse_for_function (); }
	;

/* more C++ complexity.  See component_decl for a comment on the
   reduce/reduce conflict introduced by these rules.  */
fn.def2:
	  typed_declspecs '(' parmlist ')' type_quals maybe_raises
		{
		  $$ = build_parse_node (CALL_EXPR, TREE_VALUE ($1), $3, $5);
		  $$ = start_method (TREE_CHAIN ($1), $$, $6);
		 rest_of_mdef:
		  if (! $$)
		    YYERROR1;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| typed_declspecs LEFT_RIGHT type_quals maybe_raises
		{
		  $$ = build_parse_node (CALL_EXPR, TREE_VALUE ($1),
					 empty_parms (), $3);
		  $$ = start_method (TREE_CHAIN ($1), $$, $4);
		  goto rest_of_mdef;
		}
	| typed_declspecs declarator maybe_raises
		{ $$ = start_method ($$, $2, $3); goto rest_of_mdef; }
	| declmods notype_declarator maybe_raises
		{ $$ = start_method ($$, $2, $3); goto rest_of_mdef; }
	| notype_declarator maybe_raises
		{ $$ = start_method (NULL_TREE, $$, $2); goto rest_of_mdef; }
	;

return_id: RETURN IDENTIFIER
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  $$ = $2;
		}
	;

return_init: return_id
		{ store_return_init ($<ttype>$, NULL_TREE); }
	| return_id '=' init
		{ store_return_init ($<ttype>$, $3); }
	| return_id '(' nonnull_exprlist ')'
		{ store_return_init ($<ttype>$, $3); }
	| return_id LEFT_RIGHT
		{ store_return_init ($<ttype>$, NULL_TREE); }
	;

base_init:
	  ':' .set_base_init member_init_list
		{
		  if ($3 == 0)
		    error ("no base initializers given following ':'");
		  setup_vtbl_ptr ();
		  /* Always keep the BLOCK node associated with the outermost
		     pair of curley braces of a function.  These are needed
		     for correct operation of dwarfout.c.  */
		  keep_next_level ();
		}
	;

.set_base_init:
	/* empty */
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();

		  /* Flag that we are processing base and member initializers.  */
		  current_vtable_decl = error_mark_node;

		  if (DECL_CONSTRUCTOR_P (current_function_decl))
		    {
		      /* Make a contour for the initializer list.  */
		      pushlevel (0);
		      clear_last_expr ();
		      expand_start_bindings (0);
		    }
		  else if (current_class_type == NULL_TREE)
		    error ("base initializers not allowed for non-member functions");
		  else if (! DECL_CONSTRUCTOR_P (current_function_decl))
		    error ("only constructors take base initializers");
		}
	;

member_init_list:
	  /* empty */
		{ $$ = 0; }
	| member_init
		{ $$ = 1; }
	| member_init_list ',' member_init
	| member_init_list error
	;

member_init: '(' nonnull_exprlist ')'
		{
		  if (current_class_name && !flag_traditional)
		    pedwarn ("anachronistic old style base class initializer");
		  expand_member_init (C_C_D, NULL_TREE, $2);
		}
	| LEFT_RIGHT
		{
		  if (current_class_name && !flag_traditional)
		    pedwarn ("anachronistic old style base class initializer");
		  expand_member_init (C_C_D, NULL_TREE, void_type_node);
		}
	| notype_identifier '(' nonnull_exprlist ')'
		{ expand_member_init (C_C_D, $<ttype>$, $3); }
	| notype_identifier LEFT_RIGHT
		{ expand_member_init (C_C_D, $<ttype>$, void_type_node); }
	| complete_type_name '(' nonnull_exprlist ')'
		{ expand_member_init (C_C_D, $<ttype>$, $3); }
	| complete_type_name LEFT_RIGHT
		{ expand_member_init (C_C_D, $<ttype>$, void_type_node); }
	/* GNU extension */
	| notype_qualified_id '(' nonnull_exprlist ')'
		{
		  do_member_init (OP0 ($1), OP1 ($1), $3);
		}
	| notype_qualified_id LEFT_RIGHT
		{
		  do_member_init (OP0 ($1), OP1 ($1), void_type_node);
		}
	;

identifier:
	  IDENTIFIER
	| TYPENAME
	| PTYPENAME
	;

notype_identifier:
	  IDENTIFIER
	| PTYPENAME %prec EMPTY
	;

identifier_defn:
	  IDENTIFIER_DEFN
	| TYPENAME_DEFN
	| PTYPENAME_DEFN
	;

explicit_instantiation:
	  TEMPLATE specialization template_instantiation
		{ do_type_instantiation ($3 ? $3 : $2, NULL_TREE); }
	| TEMPLATE typed_declspecs declarator
		{ do_function_instantiation ($2, $3, NULL_TREE); }
	| SCSPEC TEMPLATE specialization template_instantiation
		{ do_type_instantiation ($4 ? $4 : $3, $1); }
	| SCSPEC TEMPLATE typed_declspecs declarator
		{ do_function_instantiation ($3, $4, $1); }
	;

template_type:
	  template_type_name tmpl.2 template_instantiation
		{ if ($3) $$ = $3; }
	;

template_type_name:
	  PTYPENAME '<' template_arg_list '>'
		{ $$ = lookup_template_class ($$, $3, NULL_TREE); }
	| PTYPENAME '<' '>'
		{ $$ = lookup_template_class ($$, NULL_TREE, NULL_TREE); }
	| TYPENAME  '<' template_arg_list '>'
		{ $$ = lookup_template_class ($$, $3, NULL_TREE); }
	;

tmpl.2: 
	  /* empty */ %prec EMPTY
		{ $$ = instantiate_class_template ($<ttype>0, 1); }
	;

template_arg_list:
	  template_arg
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| template_arg_list ',' template_arg
		{ $$ = chainon ($$, build_tree_list (NULL_TREE, $3)); }
	;

template_arg:
	  type_id
		{ $$ = groktypename ($$); }
	| expr_no_commas  %prec UNARY
	;

template_instantiate_once:
	  PRE_PARSED_CLASS_DECL maybe_base_class_list
		{
		  tree t, decl, tmpl;

		  tmpl = TREE_PURPOSE (IDENTIFIER_TEMPLATE ($1));
		  t = xref_tag (DECL_TEMPLATE_INFO (tmpl)->aggr, $1, $2, 0);
		  set_current_level_tags_transparency (1);
		  my_friendly_assert (TREE_CODE (t) == RECORD_TYPE
				      || TREE_CODE (t) == UNION_TYPE, 257);
		  $<ttype>$ = t;

		  /* Now, put a copy of the decl in global scope, to avoid
		     recursive expansion.  */
		  decl = IDENTIFIER_LOCAL_VALUE ($1);
		  if (!decl)
		    decl = IDENTIFIER_CLASS_VALUE ($1);
		  /* Now, put a copy of the decl in global scope, to avoid
		     recursive expansion.  */
                  if (decl)
                    {
		      /* Need to copy it to clear the chain pointer,
			 and need to get it into permanent storage.  */
                      my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 258);
		      push_obstacks (&permanent_obstack, &permanent_obstack);
                      decl = copy_node (decl);
		      if (DECL_LANG_SPECIFIC (decl))
			copy_lang_decl (decl);
		      pop_obstacks ();
		      pushdecl_top_level (decl);
		    }
		  /* Kludge; see instantiate_class_template.  */
		  TYPE_BEING_DEFINED (t) = 0;
		}
	  left_curly opt.component_decl_list '}'
		{
		  tree t = finish_struct ($<ttype>3, $5, 0);

		  pop_obstacks ();
		  end_template_instantiation ($1);

                  /* Now go after the methods & class data.  */
                  instantiate_member_templates ($1);

		  pop_tinst_level();

		  CLASSTYPE_GOT_SEMICOLON (t) = 1;
		}
	;

template_instantiation:
          /* empty */
                { $$ = NULL_TREE; }
        | template_instantiate_once
                { $$ = $1; }
        ;

template_instantiate_some:
          /* empty */
                { $$ = NULL_TREE; /* never used from here... */}
        | template_instantiate_once template_instantiate_some
                { $$ = $1; /*???*/ }
        ;

unop:     '-'
		{ $$ = NEGATE_EXPR; }
	| '+'
		{ $$ = CONVERT_EXPR; }
	| PLUSPLUS
		{ $$ = PREINCREMENT_EXPR; }
	| MINUSMINUS
		{ $$ = PREDECREMENT_EXPR; }
	| '!'
		{ $$ = TRUTH_NOT_EXPR; }
	;

expr:	  nontrivial_exprlist
		{ $$ = build_x_compound_expr ($$); }
	| expr_no_commas
	;

paren_expr_or_null:
	LEFT_RIGHT
		{ error ("ANSI C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' expr ')'
		{ $$ = build1 (CLEANUP_POINT_EXPR, bool_type_node, 
			       bool_truthvalue_conversion ($2)); }
	;

paren_cond_or_null:
	LEFT_RIGHT
		{ error ("ANSI C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' condition ')'
		{ $$ = build1 (CLEANUP_POINT_EXPR, bool_type_node, 
			       bool_truthvalue_conversion ($2)); }
	;

xcond:
	/* empty */
		{ $$ = NULL_TREE; }
	| condition
		{ $$ = build1 (CLEANUP_POINT_EXPR, bool_type_node, 
			       bool_truthvalue_conversion ($$)); }
	| error
		{ $$ = NULL_TREE; }
	;

condition:
	type_specifier_seq declarator maybe_raises maybeasm maybe_attribute '='
		{ {
		  tree d;
		  for (d = getdecls (); d; d = TREE_CHAIN (d))
		    if (TREE_CODE (d) == TYPE_DECL) {
		      tree s = TREE_TYPE (d);
		      if (TREE_CODE (s) == RECORD_TYPE)
			cp_error ("definition of class `%T' in condition", s);
		      else if (TREE_CODE (s) == ENUMERAL_TYPE)
			cp_error ("definition of enum `%T' in condition", s);
		    }
		  }
		  current_declspecs = $1;
		  $<itype>6 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>2, current_declspecs, 1, $3);
		  cplus_decl_attributes ($<ttype>$, $5);
		}
	init
		{ 
		  finish_decl ($<ttype>7, $8, $5, 0);
		  resume_momentary ($<itype>6);
		  $$ = $<ttype>7; 
		  if (TREE_CODE (TREE_TYPE ($$)) == ARRAY_TYPE)
		    cp_error ("definition of array `%#D' in condition", $$); 
		}
	| expr
	;

already_scoped_stmt:
	  '{' '}'
		{ finish_stmt (); }
	| '{' maybe_label_decls stmts '}'
		{ finish_stmt (); }
	| '{' maybe_label_decls error '}'
		{ finish_stmt (); }
	| simple_stmt
	;


nontrivial_exprlist:
	  expr_no_commas ',' expr_no_commas
		{ $$ = tree_cons (NULL_TREE, $$, 
		                  build_tree_list (NULL_TREE, $3)); }
	| expr_no_commas ',' error
		{ $$ = tree_cons (NULL_TREE, $$, 
		                  build_tree_list (NULL_TREE, error_mark_node)); }
	| nontrivial_exprlist ',' expr_no_commas
		{ chainon ($$, build_tree_list (NULL_TREE, $3)); }
	| nontrivial_exprlist ',' error
		{ chainon ($$, build_tree_list (NULL_TREE, error_mark_node)); }
	;

nonnull_exprlist:
	  expr_no_commas
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| nontrivial_exprlist
	;

unary_expr:
	  primary %prec UNARY
		{
#if 0
		  if (TREE_CODE ($$) == TYPE_EXPR)
		    $$ = build_component_type_expr (C_C_D, $$, NULL_TREE, 1);
#endif
		}
	/* __extension__ turns off -pedantic for following primary.  */
	| EXTENSION
		{ $<itype>1 = pedantic;
		  pedantic = 0; }
	  cast_expr	  %prec UNARY
		{ $$ = $3;
		  pedantic = $<itype>1; }
	| '*' cast_expr   %prec UNARY
		{ $$ = build_x_indirect_ref ($2, "unary *"); }
	| '&' cast_expr   %prec UNARY
		{ $$ = build_x_unary_op (ADDR_EXPR, $2); }
	| '~' cast_expr
		{ $$ = build_x_unary_op (BIT_NOT_EXPR, $2); }
	| unop cast_expr  %prec UNARY
		{ $$ = build_x_unary_op ($1, $2);
		  if ($1 == NEGATE_EXPR && TREE_CODE ($2) == INTEGER_CST)
		    TREE_NEGATED_INT ($$) = 1;
		  overflow_warning ($$);
		}
	/* Refer to the address of a label as a pointer.  */
	| ANDAND identifier
		{ tree label = lookup_label ($2);
		  if (label == NULL_TREE)
		    $$ = null_pointer_node;
		  else
		    {
		      TREE_USED (label) = 1;
		      $$ = build1 (ADDR_EXPR, ptr_type_node, label);
		      TREE_CONSTANT ($$) = 1;
		    }
		}
	| SIZEOF unary_expr  %prec UNARY
		{ if (TREE_CODE ($2) == COMPONENT_REF
		      && DECL_BIT_FIELD (TREE_OPERAND ($2, 1)))
		    error ("sizeof applied to a bit-field");
		  /* ANSI says arrays and functions are converted inside comma.
		     But we can't really convert them in build_compound_expr
		     because that would break commas in lvalues.
		     So do the conversion here if operand was a comma.  */
		  if (TREE_CODE ($2) == COMPOUND_EXPR
		      && (TREE_CODE (TREE_TYPE ($2)) == ARRAY_TYPE
			  || TREE_CODE (TREE_TYPE ($2)) == FUNCTION_TYPE))
		    $2 = default_conversion ($2);
		  else if (TREE_CODE ($2) == TREE_LIST)
	            {
		      tree t = TREE_VALUE ($2);
		      if (t != NULL_TREE
			  && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
			pedwarn ("ANSI C++ forbids using sizeof() on a function");
		    }
		  $$ = c_sizeof (TREE_TYPE ($2)); }
	| SIZEOF '(' type_id ')'  %prec HYPERUNARY
		{ $$ = c_sizeof (groktypename ($3)); }
	| ALIGNOF unary_expr  %prec UNARY
		{ $$ = grok_alignof ($2); }
	| ALIGNOF '(' type_id ')'  %prec HYPERUNARY
		{ $$ = c_alignof (groktypename ($3)); }

	/* The %prec EMPTY's here are required by the = init initializer
	   syntax extension; see below.  */
	| new new_type_id %prec EMPTY
		{ $$ = build_new (NULL_TREE, $2, NULL_TREE, $1); }
	| new new_type_id new_initializer
		{ $$ = build_new (NULL_TREE, $2, $3, $1); }
	| new new_placement new_type_id %prec EMPTY
		{ $$ = build_new ($2, $3, NULL_TREE, $1); }
	| new new_placement new_type_id new_initializer
		{ $$ = build_new ($2, $3, $4, $1); }
	| new '(' type_id ')' %prec EMPTY
		{ $$ = build_new (NULL_TREE, groktypename($3),
				  NULL_TREE, $1); }
	| new '(' type_id ')' new_initializer
		{ $$ = build_new (NULL_TREE, groktypename($3), $5, $1); }
	| new new_placement '(' type_id ')' %prec EMPTY
		{ $$ = build_new ($2, groktypename($4), NULL_TREE, $1); }
	| new new_placement '(' type_id ')' new_initializer
		{ $$ = build_new ($2, groktypename($4), $6, $1); }

	| delete cast_expr  %prec UNARY
		{ $$ = delete_sanity ($2, NULL_TREE, 0, $1); }
	| delete '[' ']' cast_expr  %prec UNARY
		{ $$ = delete_sanity ($4, NULL_TREE, 1, $1);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX; }
	| delete '[' expr ']' cast_expr %prec UNARY
		{ $$ = delete_sanity ($5, $3, 2, $1);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX; }
	;

new_placement:
	  '(' nonnull_exprlist ')'
		{ $$ = $2; }
	| '{' nonnull_exprlist '}'
		{
		  $$ = $2; 
		  pedwarn ("old style placement syntax, use () instead");
		}
	;

new_initializer:
	  '(' nonnull_exprlist ')'
		{ $$ = $2; }
	| LEFT_RIGHT
		{ $$ = NULL_TREE; }
	| '(' typespec ')'
		{
		  cp_error ("`%T' is not a valid expression", $2);
		  $$ = error_mark_node;
		}
	/* GNU extension so people can use initializer lists.  Note that
	   this alters the meaning of `new int = 1', which was previously
	   syntactically valid but semantically invalid.  */
	| '=' init
		{
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids initialization of new expression with `='");
		  $$ = $2;
		}
	;

/* This is necessary to postpone reduction of `int ((int)(int)(int))'.  */
regcast_or_absdcl:
	  '(' type_id ')' %prec EMPTY
		{ $2 = tree_cons (NULL_TREE, $2, void_list_node);
		  TREE_PARMLIST ($2) = 1;
		  $$ = build_parse_node (CALL_EXPR, NULL_TREE, $2, 
					 NULL_TREE); }
	| regcast_or_absdcl '(' type_id ')' %prec EMPTY
		{ $3 = tree_cons (NULL_TREE, $3, void_list_node);
		  TREE_PARMLIST ($3) = 1;
		  $$ = build_parse_node (CALL_EXPR, $$, $3, NULL_TREE); }
	;

cast_expr:
	  sub_cast_expr
	| regcast_or_absdcl sub_cast_expr  %prec UNARY
		{ $$ = reparse_absdcl_as_casts ($$, $2); }
	| regcast_or_absdcl '{' initlist maybecomma '}'  %prec UNARY
		{ 
		  tree init = build_nt (CONSTRUCTOR, NULL_TREE,
					nreverse ($3)); 
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids constructor-expressions");
		  /* Indicate that this was a GNU C constructor expression.  */
		  TREE_HAS_CONSTRUCTOR (init) = 1;

		  $$ = reparse_absdcl_as_casts ($$, init);
		}
	;

sub_cast_expr:
	  unary_expr
	| HEADOF '(' expr ')'
		{ $$ = build_headof ($3); }
	| CLASSOF '(' expr ')'
		{ $$ = build_classof ($3); }
	| CLASSOF '(' TYPENAME ')'
		{ if (is_aggr_typedef ($3, 1))
		    {
		      tree type = IDENTIFIER_TYPE_VALUE ($3);
		      if (! IS_SIGNATURE(type))
			$$ = CLASSTYPE_DOSSIER (type);
		      else
			{
			  sorry ("signature name as argument of `classof'");
			  $$ = error_mark_node;
			}
		    }
		  else
		    $$ = error_mark_node;
		}
	;

expr_no_commas:
	  cast_expr
	/* Handle general members.  */
	| expr_no_commas POINTSAT_STAR expr_no_commas
		{ $$ = build_x_binary_op (MEMBER_REF, $$, $3); }
	| expr_no_commas DOT_STAR expr_no_commas
		{ $$ = build_m_component_ref ($$, $3); }
	| expr_no_commas '+' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '-' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '*' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '/' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '%' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas LSHIFT expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas RSHIFT expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas ARITHCOMPARE expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '<' expr_no_commas
		{ $$ = build_x_binary_op (LT_EXPR, $$, $3); }
	| expr_no_commas '>' expr_no_commas
		{ $$ = build_x_binary_op (GT_EXPR, $$, $3); }
	| expr_no_commas EQCOMPARE expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas MIN_MAX expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '&' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '|' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas '^' expr_no_commas
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_commas ANDAND expr_no_commas
		{ $$ = build_x_binary_op (TRUTH_ANDIF_EXPR, $$, $3); }
	| expr_no_commas OROR expr_no_commas
		{ $$ = build_x_binary_op (TRUTH_ORIF_EXPR, $$, $3); }
	| expr_no_commas '?' xexpr ':' expr_no_commas
		{ $$ = build_x_conditional_expr ($$, $3, $5); }
	| expr_no_commas '=' expr_no_commas
		{ $$ = build_modify_expr ($$, NOP_EXPR, $3); }
	| expr_no_commas ASSIGN expr_no_commas
		{ register tree rval;
		  if ((rval = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, $$, $3,
					     make_node ($2))))
		    $$ = rval;
		  else
		    $$ = build_modify_expr ($$, $2, $3); }
	| THROW
		{ $$ = build_throw (NULL_TREE); }
	| THROW expr_no_commas
		{ $$ = build_throw ($2); }
/* These extensions are not defined.  The second arg to build_m_component_ref
   is old, build_m_component_ref now does an implicit
   build_indirect_ref (x, NULL_PTR) on the second argument.
	| object '&' expr_no_commas   %prec UNARY
		{ $$ = build_m_component_ref ($$, build_x_unary_op (ADDR_EXPR, $3)); }
	| object unop expr_no_commas  %prec UNARY
		{ $$ = build_m_component_ref ($$, build_x_unary_op ($2, $3)); }
	| object '(' type_id ')' expr_no_commas  %prec UNARY
		{ tree type = groktypename ($3);
		  $$ = build_m_component_ref ($$, build_c_cast (type, $5)); }
	| object primary_no_id  %prec UNARY
		{ $$ = build_m_component_ref ($$, $2); }
*/
	;

notype_unqualified_id:
	  '~' see_typename identifier
		{ $$ = build_parse_node (BIT_NOT_EXPR, $3); }
	| operator_name
	| IDENTIFIER
	| PTYPENAME %prec EMPTY
	;

unqualified_id:
	  notype_unqualified_id
	| TYPENAME
	;

expr_or_declarator:
	  notype_unqualified_id
	| notype_qualified_id
	| '*' expr_or_declarator %prec UNARY
		{ $$ = build_parse_node (INDIRECT_REF, $2); }
	| '&' expr_or_declarator %prec UNARY
		{ $$ = build_parse_node (ADDR_EXPR, $2); }
	;

direct_notype_declarator:
	  complex_direct_notype_declarator
	| notype_unqualified_id
	| notype_qualified_id
		{ push_nested_class (TREE_TYPE (OP0 ($$)), 3);
		  TREE_COMPLEXITY ($$) = current_class_depth; }
	;

primary:
	  notype_unqualified_id
		{
		  if (TREE_CODE ($$) == BIT_NOT_EXPR)
		    $$ = build_x_unary_op (BIT_NOT_EXPR, TREE_OPERAND ($$, 0));
		  else if (IDENTIFIER_OPNAME_P ($$))
		    {
		      tree op = $$;
		      $$ = lookup_name (op, 0);
		      if ($$ == NULL_TREE)
			{
			  if (op != ansi_opname[ERROR_MARK])
			    error ("operator %s not defined",
				   operator_name_string (op));
			  $$ = error_mark_node;
			}
		    }
		  else
		    $$ = do_identifier ($$);
		}		
	| CONSTANT
	| boolean.literal
	| string
		{ $$ = combine_strings ($$); }
	| '(' expr ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| '('
		{ if (current_function_decl == 0)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  keep_next_level ();
		  $<ttype>$ = expand_start_stmt_expr (); }
	  compstmt ')'
		{ tree rtl_exp;
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids braced-groups within expressions");
		  rtl_exp = expand_end_stmt_expr ($<ttype>2);
		  /* The statements have side effects, so the group does.  */
		  TREE_SIDE_EFFECTS (rtl_exp) = 1;

		  if (TREE_CODE ($3) == BLOCK)
		    {
		      /* Make a BIND_EXPR for the BLOCK already made.  */
		      $$ = build (BIND_EXPR, TREE_TYPE (rtl_exp),
				  NULL_TREE, rtl_exp, $3);
		      /* Remove the block from the tree at this point.
			 It gets put back at the proper place
			 when the BIND_EXPR is expanded.  */
		      delete_block ($3);
		    }
		  else
		    $$ = $3;
		}
	| primary '(' nonnull_exprlist ')'
                { /* [eichin:19911016.1902EST] */
                  $<ttype>$ = build_x_function_call ($1, $3, current_class_decl); 
                  /* here we instantiate_class_template as needed... */
                  do_pending_templates ();
                } template_instantiate_some {
                  if (TREE_CODE ($<ttype>5) == CALL_EXPR
                      && TREE_TYPE ($<ttype>5) != void_type_node)
	            $$ = require_complete_type ($<ttype>5);
                  else
                    $$ = $<ttype>5;
                }
	| primary LEFT_RIGHT
                {
		  $$ = build_x_function_call ($$, NULL_TREE, current_class_decl);
		  if (TREE_CODE ($$) == CALL_EXPR
		      && TREE_TYPE ($$) != void_type_node)
		    $$ = require_complete_type ($$);
                }
	| primary '[' expr ']'
		{ $$ = grok_array_decl ($$, $3); }
	| primary PLUSPLUS
		{ /* If we get an OFFSET_REF, turn it into what it really
		     means (e.g., a COMPONENT_REF).  This way if we've got,
		     say, a reference to a static member that's being operated
		     on, we don't end up trying to find a member operator for
		     the class it's in.  */
		  if (TREE_CODE ($$) == OFFSET_REF)
		    $$ = resolve_offset_ref ($$);
		  $$ = build_x_unary_op (POSTINCREMENT_EXPR, $$); }
	| primary MINUSMINUS
		{ if (TREE_CODE ($$) == OFFSET_REF)
		    $$ = resolve_offset_ref ($$);
		  $$ = build_x_unary_op (POSTDECREMENT_EXPR, $$); }
	/* C++ extensions */
	| THIS
		{ if (current_class_decl)
		    {
#ifdef WARNING_ABOUT_CCD
		      TREE_USED (current_class_decl) = 1;
#endif
		      $$ = current_class_decl;
		    }
		  else if (current_function_decl
			   && DECL_STATIC_FUNCTION_P (current_function_decl))
		    {
		      error ("`this' is unavailable for static member functions");
		      $$ = error_mark_node;
		    }
		  else
		    {
		      if (current_function_decl)
			error ("invalid use of `this' in non-member function");
		      else
			error ("invalid use of `this' at top level");
		      $$ = error_mark_node;
		    }
		}
	| TYPE_QUAL '(' nonnull_exprlist ')'
		{
		  tree type;
		  tree id = $$;

		  /* This is a C cast in C++'s `functional' notation.  */
		  if ($3 == error_mark_node)
		    {
		      $$ = error_mark_node;
		      break;
		    }
#if 0
		  if ($3 == NULL_TREE)
		    {
		      error ("cannot cast null list to type `%s'",
		             IDENTIFIER_POINTER (TYPE_NAME (id)));
		      $$ = error_mark_node;
		      break;
		    }
#endif
#if 0
		  /* type is not set! (mrs) */
		  if (type == error_mark_node)
		    $$ = error_mark_node;
		  else
#endif
		    {
		      if (id == ridpointers[(int) RID_CONST])
		        type = build_type_variant (integer_type_node, 1, 0);
		      else if (id == ridpointers[(int) RID_VOLATILE])
		        type = build_type_variant (integer_type_node, 0, 1);
#if 0
		      /* should not be able to get here (mrs) */
		      else if (id == ridpointers[(int) RID_FRIEND])
		        {
		          error ("cannot cast expression to `friend' type");
		          $$ = error_mark_node;
		          break;
		        }
#endif
		      else my_friendly_abort (79);
		      $$ = build_c_cast (type, build_compound_expr ($3));
		    }
		}
	| functional_cast
	| DYNAMIC_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3);
		  $$ = build_dynamic_cast (type, $6); }
	| STATIC_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3);
		  $$ = build_static_cast (type, $6); }
	| REINTERPRET_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3);
		  $$ = build_reinterpret_cast (type, $6); }
	| CONST_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3);
		  $$ = build_const_cast (type, $6); }
	| TYPEID '(' expr ')'
		{ $$ = build_typeid ($3); }
	| TYPEID '(' type_id ')'
		{ tree type = groktypename ($3);
		  $$ = get_typeid (type); }
	| global_scope IDENTIFIER
		{
		do_scoped_id:
		  $$ = IDENTIFIER_GLOBAL_VALUE ($2);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  if (! $$)
		    {
		      if (yychar == '(' || yychar == LEFT_RIGHT)
			$$ = implicitly_declare ($2);
		      else
			{
			  if (IDENTIFIER_GLOBAL_VALUE ($2) != error_mark_node)
			    error ("undeclared variable `%s' (first use here)",
				   IDENTIFIER_POINTER ($2));
			  $$ = error_mark_node;
			  /* Prevent repeated error messages.  */
			  IDENTIFIER_GLOBAL_VALUE ($2) = error_mark_node;
			}
		    }
		  else
		    {
		      if (TREE_CODE ($$) == ADDR_EXPR)
			assemble_external (TREE_OPERAND ($$, 0));
		      else
			assemble_external ($$);
		      TREE_USED ($$) = 1;
		    }
		  if (TREE_CODE ($$) == CONST_DECL)
		    {
		      /* XXX CHS - should we set TREE_USED of the constant? */
		      $$ = DECL_INITIAL ($$);
		      /* This is to prevent an enum whose value is 0
			 from being considered a null pointer constant.  */
		      $$ = build1 (NOP_EXPR, TREE_TYPE ($$), $$);
		      TREE_CONSTANT ($$) = 1;
		    }

		}
	| global_scope operator_name
		{
		  got_scope = NULL_TREE;
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    goto do_scoped_id;
		  $$ = $2;
		}
	| overqualified_id %prec HYPERUNARY
		{ $$ = build_offset_ref (OP0 ($$), OP1 ($$)); }
	| overqualified_id '(' nonnull_exprlist ')'
		{ $$ = build_member_call (OP0 ($$), OP1 ($$), $3); }
	| overqualified_id LEFT_RIGHT
		{ $$ = build_member_call (OP0 ($$), OP1 ($$), NULL_TREE); }
	| object unqualified_id  %prec UNARY
		{ $$ = build_component_ref ($$, $2, NULL_TREE, 1); }
	| object qualified_id %prec UNARY
		{ $$ = build_object_ref ($$, OP0 ($2), OP1 ($2)); }
	| object unqualified_id '(' nonnull_exprlist ')'
		{
#if 0
		  /* This is a future direction of this code, but because
		     build_x_function_call cannot always undo what is done
		     in build_component_ref entirely yet, we cannot do this. */
		  $$ = build_x_function_call (build_component_ref ($$, $2, NULL_TREE, 1), $4, $$);
		  if (TREE_CODE ($$) == CALL_EXPR
		      && TREE_TYPE ($$) != void_type_node)
		    $$ = require_complete_type ($$);
#else
		  $$ = build_method_call ($$, $2, $4, NULL_TREE,
					  (LOOKUP_NORMAL|LOOKUP_AGGR));
#endif
		}
	| object unqualified_id LEFT_RIGHT
		{
#if 0
		  /* This is a future direction of this code, but because
		     build_x_function_call cannot always undo what is done
		     in build_component_ref entirely yet, we cannot do this. */
		  $$ = build_x_function_call (build_component_ref ($$, $2, NULL_TREE, 1), NULL_TREE, $$);
		  if (TREE_CODE ($$) == CALL_EXPR
		      && TREE_TYPE ($$) != void_type_node)
		    $$ = require_complete_type ($$);
#else
		  $$ = build_method_call ($$, $2, NULL_TREE, NULL_TREE,
					  (LOOKUP_NORMAL|LOOKUP_AGGR));
#endif
		}
	| object qualified_id '(' nonnull_exprlist ')'
		{
		  if (IS_SIGNATURE (IDENTIFIER_TYPE_VALUE (OP0 ($2))))
		    {
		      warning ("signature name in scope resolution ignored");
		      $$ = build_method_call ($$, OP1 ($2), $4, NULL_TREE,
					      (LOOKUP_NORMAL|LOOKUP_AGGR));
		    }
		  else
		    $$ = build_scoped_method_call ($$, OP0 ($2), OP1 ($2), $4);
		}
	| object qualified_id LEFT_RIGHT
		{
		  if (IS_SIGNATURE (IDENTIFIER_TYPE_VALUE (OP0 ($2))))
		    {
		      warning ("signature name in scope resolution ignored");
		      $$ = build_method_call ($$, OP1 ($2), NULL_TREE, NULL_TREE,
					      (LOOKUP_NORMAL|LOOKUP_AGGR));
		    }
		  else
		    $$ = build_scoped_method_call ($$, OP0 ($2), OP1 ($2), NULL_TREE);
		}
	/* p->int::~int() is valid -- 12.4 */
	| object '~' TYPESPEC LEFT_RIGHT
		{ 
		  if (TREE_CODE (TREE_TYPE ($1)) 
		      != TREE_CODE (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE ($3))))
		    cp_error ("`%E' is not of type `%T'", $1, $3);
		  $$ = convert (void_type_node, $1);
		}
	| object TYPESPEC SCOPE '~' TYPESPEC LEFT_RIGHT
		{ 
		  if ($2 != $5)
		    cp_error ("destructor specifier `%T::~%T()' must have matching names", $2, $5);
		  if (TREE_CODE (TREE_TYPE ($1))
		      != TREE_CODE (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE ($2))))
		    cp_error ("`%E' is not of type `%T'", $1, $2);
		  $$ = convert (void_type_node, $1);
		}
	;

/* Not needed for now.

primary_no_id:
	  '(' expr ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| '('
		{ if (current_function_decl == 0)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  $<ttype>$ = expand_start_stmt_expr (); }
	  compstmt ')'
		{ if (flag_ansi)
		    pedwarn ("ANSI C++ forbids braced-groups within expressions");
		  $$ = expand_end_stmt_expr ($<ttype>2); }
	| primary_no_id '(' nonnull_exprlist ')'
		{ $$ = build_x_function_call ($$, $3, current_class_decl); }
	| primary_no_id LEFT_RIGHT
		{ $$ = build_x_function_call ($$, NULL_TREE, current_class_decl); }
	| primary_no_id '[' expr ']'
		{ goto do_array; }
	| primary_no_id PLUSPLUS
		{ $$ = build_x_unary_op (POSTINCREMENT_EXPR, $$); }
	| primary_no_id MINUSMINUS
		{ $$ = build_x_unary_op (POSTDECREMENT_EXPR, $$); }
	| SCOPE IDENTIFIER
		{ goto do_scoped_id; }
	| SCOPE operator_name
		{ if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    goto do_scoped_id;
		  goto do_scoped_operator;
		}
	;
*/

new:	  NEW
		{ $$ = 0; }
	| global_scope NEW
		{ got_scope = NULL_TREE; $$ = 1; }
	;

delete:	  DELETE
		{ $$ = 0; }
	| global_scope delete
		{ got_scope = NULL_TREE; $$ = 1; }
	;

boolean.literal:
	  CXX_TRUE
		{ $$ = true_node; }
	| CXX_FALSE
		{ $$ = false_node; }
	;

/* Produces a STRING_CST with perhaps more STRING_CSTs chained onto it.  */
string:
	  STRING
	| string STRING
		{ $$ = chainon ($$, $2); }
	;

nodecls:
	  /* empty */
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  setup_vtbl_ptr ();
		  /* Always keep the BLOCK node associated with the outermost
		     pair of curley braces of a function.  These are needed
		     for correct operation of dwarfout.c.  */
		  keep_next_level ();
		}
	;

object:	  primary '.'
	| primary POINTSAT
		{
		  $$ = build_x_arrow ($$);
		}
	;

decl:
	/* Normal case: make this fast.  */
	  typespec declarator ';'
		{ tree d = get_decl_list ($1);
		  int yes = suspend_momentary ();
		  d = start_decl ($2, d, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE, 0);
		  resume_momentary (yes);
		  if (IS_AGGR_TYPE_CODE (TREE_CODE ($1)))
		    note_got_semicolon ($1);
		}
	| typed_declspecs declarator ';'
		{ tree d = $1;
		  int yes = suspend_momentary ();
		  d = start_decl ($2, d, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE, 0);
		  resume_momentary (yes);
		  note_list_got_semicolon ($1);
		}
	| typespec initdecls ';'
		{
		  resume_momentary ($2);
		  if (IS_AGGR_TYPE_CODE (TREE_CODE ($1)))
		    note_got_semicolon ($1);
		}
	| typed_declspecs initdecls ';'
		{
		  resume_momentary ($2);
		  note_list_got_semicolon ($1);
		}
	| declmods notype_initdecls ';'
		{ resume_momentary ($2); }
	| typed_declspecs ';'
		{
		  shadow_tag ($1);
		  note_list_got_semicolon ($1);
		}
	| declmods ';'
		{ warning ("empty declaration"); }
	;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator %prec EMPTY
	| notype_declarator %prec EMPTY
	;

/* This is necessary to postpone reduction of `int()()()()'.  */
fcast_or_absdcl:
	  LEFT_RIGHT %prec EMPTY
		{ $$ = build_parse_node (CALL_EXPR, NULL_TREE, empty_parms (),
					 NULL_TREE); }
	| fcast_or_absdcl LEFT_RIGHT %prec EMPTY
		{ $$ = build_parse_node (CALL_EXPR, $$, empty_parms (), 
					 NULL_TREE); }
	;

/* ANSI type-id (8.1) */
type_id:
	  typed_typespecs absdcl
		{ $$ = build_decl_list ($$, $2); }
	| nonempty_type_quals absdcl
		{ $$ = build_decl_list ($$, $2); }
	| typespec absdcl
		{ $$ = build_decl_list (get_decl_list ($$), $2); }
	| typed_typespecs %prec EMPTY
		{ $$ = build_decl_list ($$, NULL_TREE); }
	| nonempty_type_quals %prec EMPTY
		{ $$ = build_decl_list ($$, NULL_TREE); }
	;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.  */

typed_declspecs:
	  typed_typespecs %prec EMPTY
	| typed_declspecs1

typed_declspecs1:
	  declmods typespec
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	| typespec reserved_declspecs	%prec HYPERUNARY
		{ $$ = decl_tree_cons (NULL_TREE, $$, $2); }
	| declmods typespec reserved_declspecs
		{ $$ = decl_tree_cons (NULL_TREE, $2, chainon ($3, $$)); }
	| declmods typespec reserved_typespecquals
		{ $$ = decl_tree_cons (NULL_TREE, $2, chainon ($3, $$)); }
	| declmods typespec reserved_typespecquals reserved_declspecs
		{ $$ = decl_tree_cons (NULL_TREE, $2, 
				       chainon ($3, chainon ($4, $$))); }
	;

reserved_declspecs:
	  SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($$));
		  $$ = build_decl_list (NULL_TREE, $$); }
	| reserved_declspecs typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	| reserved_declspecs SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.  */

declmods:
	  nonempty_type_quals %prec EMPTY
		{ TREE_STATIC ($$) = 1; }
	| SCSPEC
		{ $$ = IDENTIFIER_AS_LIST ($$); }
	| declmods TYPE_QUAL
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$);
		  TREE_STATIC ($$) = 1; }
	| declmods SCSPEC
		{ if (extra_warnings && TREE_STATIC ($$))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$ = decl_tree_cons (NULL_TREE, $2, $$);
		  TREE_STATIC ($$) = TREE_STATIC ($1); }
	;


/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  typespec  %prec EMPTY
		{ $$ = get_decl_list ($$); }
	| nonempty_type_quals typespec
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	| typespec reserved_typespecquals
		{ $$ = decl_tree_cons (NULL_TREE, $$, $2); }
	| nonempty_type_quals typespec reserved_typespecquals
		{ $$ = decl_tree_cons (NULL_TREE, $2, chainon ($3, $$)); }
	;

reserved_typespecquals:
	  typespecqual_reserved
		{ $$ = build_decl_list (NULL_TREE, $$); }
	| reserved_typespecquals typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec: structsp
	| TYPESPEC  %prec EMPTY
	| complete_type_name
	| TYPEOF '(' expr ')'
		{ $$ = TREE_TYPE ($3);
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids `typeof'"); }
	| TYPEOF '(' type_id ')'
		{ $$ = groktypename ($3);
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids `typeof'"); }
	| SIGOF '(' expr ')'
		{ tree type = TREE_TYPE ($3);

		  if (IS_AGGR_TYPE (type))
		    {
		      sorry ("sigof type specifier");
		      $$ = type;
		    }
		  else
		    {
		      error ("`sigof' applied to non-aggregate expression");
		      $$ = error_mark_node;
		    }
		}
	| SIGOF '(' type_id ')'
		{ tree type = groktypename ($3);

		  if (IS_AGGR_TYPE (type))
		    {
		      sorry ("sigof type specifier");
		      $$ = type;
		    }
		  else
		    {
		      error("`sigof' applied to non-aggregate type");
		      $$ = error_mark_node;
		    }
		}
	;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved: TYPESPEC
	| TYPE_QUAL
	| structsp
	;

initdecls:
	  initdcl0
	| initdecls ',' initdcl
	;

notype_initdecls:
	  notype_initdcl0
	| notype_initdecls ',' initdcl
	;

nomods_initdecls:
	  nomods_initdcl0
	| nomods_initdecls ',' initdcl
	;

maybeasm:
	  /* empty */
		{ $$ = NULL_TREE; }
	| asm_keyword '(' string ')'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3); $$ = $3; }
	;

initdcl0:
	  declarator maybe_raises maybeasm maybe_attribute '='
		{ current_declspecs = $<ttype>0;
		  if (TREE_CODE (current_declspecs) != TREE_LIST)
		    current_declspecs = get_decl_list (current_declspecs);
		  if (have_extern_spec && !used_extern_spec)
		    {
		      current_declspecs = decl_tree_cons
			(NULL_TREE, get_identifier ("extern"), 
			 current_declspecs);
		      used_extern_spec = 1;
		    }
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1, $2);
		  cplus_decl_attributes ($<ttype>$, $4); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3, 0);
		  $$ = $<itype>5; }
	| declarator maybe_raises maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = $<ttype>0;
		  if (TREE_CODE (current_declspecs) != TREE_LIST)
		    current_declspecs = get_decl_list (current_declspecs);
		  if (have_extern_spec && !used_extern_spec)
		    {
		      current_declspecs = decl_tree_cons
			(NULL_TREE, get_identifier ("extern"), 
			 current_declspecs);
		      used_extern_spec = 1;
		    }
		  $$ = suspend_momentary ();
		  d = start_decl ($<ttype>1, current_declspecs, 0, $2);
		  cplus_decl_attributes (d, $4);
		  finish_decl (d, NULL_TREE, $3, 0); }
	;

initdcl:
	  declarator maybe_raises maybeasm maybe_attribute '='
		{ $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1, $2);
		  cplus_decl_attributes ($<ttype>$, $4); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3, 0); }
	| declarator maybe_raises maybeasm maybe_attribute
		{ $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 0, $2);
		  cplus_decl_attributes ($<ttype>$, $4);
		  finish_decl ($<ttype>$, NULL_TREE, $3, 0); }
	;

notype_initdcl0:
	  notype_declarator maybe_raises maybeasm maybe_attribute '='
		{ current_declspecs = $<ttype>0;
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1, $2);
		  cplus_decl_attributes ($<ttype>$, $4); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3, 0);
		  $$ = $<itype>5; }
	| notype_declarator maybe_raises maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = $<ttype>0;
		  $$ = suspend_momentary ();
		  d = start_decl ($<ttype>1, current_declspecs, 0, $2);
		  cplus_decl_attributes (d, $4);
		  finish_decl (d, NULL_TREE, $3, 0); }
	;

nomods_initdcl0:
	  notype_declarator maybe_raises maybeasm maybe_attribute '='
		{ current_declspecs = NULL_TREE;
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($1, current_declspecs, 1, $2);
		  cplus_decl_attributes ($<ttype>$, $4); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3, 0);
		  $$ = $<itype>5; }
	| notype_declarator maybe_raises maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = NULL_TREE;
		  $$ = suspend_momentary ();
		  d = start_decl ($1, current_declspecs, 0, $2);
		  cplus_decl_attributes (d, $4);
		  finish_decl (d, NULL_TREE, $3, 0); }
	;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile. */
maybe_attribute:
      /* empty */
  		{ $$ = NULL_TREE; }
	| attributes
		{ $$ = $1; }
	;
 
attributes:
      attribute
		{ $$ = $1; }
	| attributes attribute
		{ $$ = chainon ($1, $2); }
	;

attribute:
      ATTRIBUTE '(' '(' attribute_list ')' ')'
		{ $$ = $4; }
	;

attribute_list:
      attrib
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| attribute_list ',' attrib
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;
 
attrib:
    /* empty */
		{ $$ = NULL_TREE; }
	| any_word
		{ $$ = $1; }
	| any_word '(' IDENTIFIER ')'
		{ $$ = tree_cons ($1, NULL_TREE, build_tree_list (NULL_TREE, $3)); }
	| any_word '(' IDENTIFIER ',' nonnull_exprlist ')'
		{ $$ = tree_cons ($1, NULL_TREE, tree_cons (NULL_TREE, $3, $5)); }
	| any_word '(' nonnull_exprlist ')'
		{ $$ = tree_cons ($1, NULL_TREE, $3); }
	;

/* This still leaves out most reserved keywords,
   shouldn't we include them?  */

any_word:
	  identifier
	| SCSPEC
	| TYPESPEC
	| TYPE_QUAL
	;

/* A nonempty list of identifiers, including typenames.  */
identifiers_or_typenames:
	identifier
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| identifiers_or_typenames ',' identifier
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;

init:
	  expr_no_commas %prec '='
	| '{' '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, NULL_TREE);
		  TREE_HAS_CONSTRUCTOR ($$) = 1; }
	| '{' initlist '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, nreverse ($2));
		  TREE_HAS_CONSTRUCTOR ($$) = 1; }
	| '{' initlist ',' '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, nreverse ($2));
		  TREE_HAS_CONSTRUCTOR ($$) = 1; }
	| error
		{ $$ = NULL_TREE; }
	;

/* This chain is built in reverse order,
   and put in forward order where initlist is used.  */
initlist:
	  init
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| initlist ',' init
		{ $$ = tree_cons (NULL_TREE, $3, $$); }
	/* These are for labeled elements.  */
	| '[' expr_no_commas ']' init
		{ $$ = build_tree_list ($2, $4); }
	| initlist ',' CASE expr_no_commas ':' init
		{ $$ = tree_cons ($4, $6, $$); }
	| identifier ':' init
		{ $$ = build_tree_list ($$, $3); }
	| initlist ',' identifier ':' init
		{ $$ = tree_cons ($3, $5, $$); }
	;

structsp:
	  ENUM identifier '{'
		{ $<itype>3 = suspend_momentary ();
		  $$ = start_enum ($2); }
	  enumlist maybecomma_warn '}'
		{ $$ = finish_enum ($<ttype>4, $5);
		  resume_momentary ((int) $<itype>3);
		  check_for_missing_semicolon ($<ttype>4); }
	| ENUM identifier '{' '}'
		{ $$ = finish_enum (start_enum ($2), NULL_TREE);
		  check_for_missing_semicolon ($$); }
	| ENUM '{'
		{ $<itype>2 = suspend_momentary ();
		  $$ = start_enum (make_anon_name ()); }
	  enumlist maybecomma_warn '}'
		{ $$ = finish_enum ($<ttype>3, $4);
		  resume_momentary ((int) $<itype>1);
		  check_for_missing_semicolon ($<ttype>3); }
	| ENUM '{' '}'
		{ $$ = finish_enum (start_enum (make_anon_name()), NULL_TREE);
		  check_for_missing_semicolon ($$); }
	| ENUM identifier
		{ $$ = xref_tag (enum_type_node, $2, NULL_TREE, 0); }
	| ENUM complex_type_name
		{ $$ = xref_tag (enum_type_node, $2, NULL_TREE, 0); }

	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head left_curly opt.component_decl_list '}'
		{
		  int semi;
		  tree id;

#if 0
		  /* Need to rework class nesting in the
		     presence of nested classes, etc.  */
		  shadow_tag (CLASSTYPE_AS_LIST ($$)); */
#endif
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  semi = yychar == ';';
		  /* finish_struct nukes this anyway; if
		     finish_exception does too, then it can go. */
		  if (semi)
		    note_got_semicolon ($$);

		  if (TREE_CODE ($$) == ENUMERAL_TYPE)
		    /* $$ = $1 from default rule.  */;
		  else if (CLASSTYPE_DECLARED_EXCEPTION ($$))
		    {
		    }
		  else
		    {
		      $$ = finish_struct ($$, $3, semi);
		      if (semi) note_got_semicolon ($$);
		    }

		  pop_obstacks ();

		  id = TYPE_IDENTIFIER ($$);
		  if (id && IDENTIFIER_TEMPLATE (id))
		    {
		      tree decl;

		      /* I don't know if the copying of this TYPE_DECL is
		       * really needed.  However, it's such a small per-
		       * formance penalty that the extra safety is a bargain.
		       * - niklas@appli.se
		       */
		      push_obstacks (&permanent_obstack, &permanent_obstack);
		      decl = copy_node (lookup_name (id, 0));
		      if (DECL_LANG_SPECIFIC (decl))
			copy_lang_decl (decl);
		      pop_obstacks ();
		      undo_template_name_overload (id, 0);
		      pushdecl_top_level (decl);
		    }
		  if (! semi)
		    check_for_missing_semicolon ($$); }
	| class_head  %prec EMPTY
		{
#if 0
  /* It's no longer clear what the following error is supposed to
     accomplish.  If it turns out to be needed, add a comment why.  */
		  if (TYPE_BINFO_BASETYPES ($$) && !TYPE_SIZE ($$))
		    {
		      error ("incomplete definition of type `%s'",
			     TYPE_NAME_STRING ($$));
		      $$ = error_mark_node;
		    }
#endif
		}
	;

maybecomma:
	  /* empty */
	| ','
	;

maybecomma_warn:
	  /* empty */
	| ','
		{ if (pedantic) pedwarn ("comma at end of enumerator list"); }
	;

aggr:	  AGGR
	| aggr SCSPEC
		{ error ("storage class specifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr TYPESPEC
		{ error ("type specifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr TYPE_QUAL
		{ error ("type qualifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr AGGR
		{ error ("no body nor ';' separates two class, struct or union declarations"); }
	;

specialization:
	  aggr template_type_name ';'
		{ 
		  yyungetc (';', 1); current_aggr = $$; $$ = $2; 
		  if ($<ttype>0 == ridpointers[(int) RID_TEMPLATE])
		    instantiate_class_template ($$, 2);
		}
	;

named_class_head_sans_basetype:
	  aggr identifier
		{ current_aggr = $$; $$ = $2; }
	| aggr complex_type_name
		{ current_aggr = $$; $$ = $2; }
	| aggr template_type %prec EMPTY
		{ current_aggr = $$; $$ = $2; }
	| aggr template_type_name '{'
		{ yyungetc ('{', 1);
		aggr2:
		  current_aggr = $$;
		  $$ = $2;
		  overload_template_name ($$, 0); }
	| aggr template_type_name ':'
		{ yyungetc (':', 1); goto aggr2; }
	| specialization
	;

named_class_head_sans_basetype_defn:
	  aggr identifier_defn %prec EMPTY
		{ current_aggr = $$; $$ = $2; }
	;

do_xref: /* empty */ %prec EMPTY
	{ $<ttype>$ = xref_tag (current_aggr, $<ttype>0, NULL_TREE, 1); }

do_xref_defn: /* empty */ %prec EMPTY
	{ $<ttype>$ = xref_defn_tag (current_aggr, $<ttype>0, NULL_TREE); }

named_class_head:
	  named_class_head_sans_basetype do_xref
	  maybe_base_class_list %prec EMPTY
		{
		  if ($3)
		    $$ = xref_tag (current_aggr, $1, $3, 1);
		  else
		    $$ = $<ttype>2;
		}
	|
	  named_class_head_sans_basetype_defn do_xref_defn
	  maybe_base_class_list %prec EMPTY
		{
		  if ($3)
		    $$ = xref_defn_tag (current_aggr, $1, $3);
		  else
		    $$ = $<ttype>2;
		}
	;

unnamed_class_head: aggr '{'
		{ $$ = xref_tag ($$, make_anon_name (), NULL_TREE, 0);
		  yyungetc ('{', 1); }
	;

class_head: unnamed_class_head | named_class_head ;

maybe_base_class_list:
	  %prec EMPTY /* empty */
		{ $$ = NULL_TREE; }
	| ':'  %prec EMPTY
		{ yyungetc(':', 1); $$ = NULL_TREE; }
	| ':' base_class_list  %prec EMPTY
		{ $$ = $2; }
	;

base_class_list:
	  base_class
	| base_class_list ',' base_class
		{ $$ = chainon ($$, $3); }
	;

base_class:
	  base_class.1
		{
		  tree type;
		 do_base_class1:
		  type = IDENTIFIER_TYPE_VALUE ($$);
		  if (! is_aggr_typedef ($$, 1))
		    $$ = NULL_TREE;
		  else if (current_aggr == signature_type_node
			   && (! type) && (! IS_SIGNATURE (type)))
		    {
		      error ("class name not allowed as base signature");
		      $$ = NULL_TREE;
		    }
		  else if (current_aggr == signature_type_node)
		    {
		      sorry ("signature inheritance, base type `%s' ignored",
			     IDENTIFIER_POINTER ($$));
		      $$ = build_tree_list ((tree)access_public, $$);
		    }
		  else if (type && IS_SIGNATURE (type))
		    {
		      error ("signature name not allowed as base class");
		      $$ = NULL_TREE;
		    }
		  else
		    $$ = build_tree_list ((tree)access_default, $$);
		}
	| base_class_access_list base_class.1
		{
		  tree type;
		 do_base_class2:
		  type = IDENTIFIER_TYPE_VALUE ($2);
		  if (current_aggr == signature_type_node)
		    error ("access and source specifiers not allowed in signature");
		  if (! is_aggr_typedef ($2, 1))
		    $$ = NULL_TREE;
		  else if (current_aggr == signature_type_node
			   && (! type) && (! IS_SIGNATURE (type)))
		    {
		      error ("class name not allowed as base signature");
		      $$ = NULL_TREE;
		    }
		  else if (current_aggr == signature_type_node)
		    {
		      sorry ("signature inheritance, base type `%s' ignored",
			     IDENTIFIER_POINTER ($$));
		      $$ = build_tree_list ((tree)access_public, $2);
		    }
		  else if (type && IS_SIGNATURE (type))
		    {
		      error ("signature name not allowed as base class");
		      $$ = NULL_TREE;
		    }
		  else
		    $$ = build_tree_list ((tree) $$, $2);
		}
	;

base_class.1:
	  complete_type_name
	| SIGOF '(' expr ')'
		{
		  if (current_aggr == signature_type_node)
		    {
		      if (IS_AGGR_TYPE (TREE_TYPE ($3)))
			{
			  sorry ("`sigof' as base signature specifier");
			  /* need to return some dummy signature identifier */
			  $$ = $3;
			}
		      else
			{
			  error ("`sigof' applied to non-aggregate expression");
			  $$ = error_mark_node;
			}
		    }
		  else
		    {
		      error ("`sigof' in struct or class declaration");
		      $$ = error_mark_node;
		    }
		}
	| SIGOF '(' type_id ')'
		{
		  if (current_aggr == signature_type_node)
		    {
		      if (IS_AGGR_TYPE (groktypename ($3)))
			{
			  sorry ("`sigof' as base signature specifier");
			  /* need to return some dummy signature identifier */
			  $$ = $3;
			}
		      else
			{
			  error ("`sigof' applied to non-aggregate expression");
			  $$ = error_mark_node;
			}
		    }
		  else
		    {
		      error ("`sigof' in struct or class declaration");
		      $$ = error_mark_node;
		    }
		}
	;

base_class_access_list:
	  VISSPEC
	| SCSPEC
		{ if ($<ttype>$ != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual access");
		  $$ = access_default_virtual; }
	| base_class_access_list VISSPEC
		{ int err = 0;
		  if ($2 == access_protected)
		    {
		      warning ("`protected' access not implemented");
		      $2 = access_public;
		      err++;
		    }
		  else if ($2 == access_public)
		    {
		      if ($1 == access_private)
			{
			mixed:
			  error ("base class cannot be public and private");
			}
		      else if ($1 == access_default_virtual)
			$$ = access_public_virtual;
		    }
		  else /* $2 == access_private */
		    {
		      if ($1 == access_public)
			goto mixed;
		      else if ($1 == access_default_virtual)
			$$ = access_private_virtual;
		    }
		}
	| base_class_access_list SCSPEC
		{ if ($2 != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual access");
		  if ($$ == access_public)
		    $$ = access_public_virtual;
		  else if ($$ == access_private)
		    $$ = access_private_virtual; }
	;

left_curly: '{'
		{ tree t = $<ttype>0;
		  push_obstacks_nochange ();
		  end_temporary_allocation ();

		  if (! IS_AGGR_TYPE (t))
		    {
		      t = $<ttype>0 = make_lang_type (RECORD_TYPE);
		      TYPE_NAME (t) = get_identifier ("erroneous type");
		    }
		  if (TYPE_SIZE (t))
		    duplicate_tag_error (t);
                  if (TYPE_SIZE (t) || TYPE_BEING_DEFINED (t))
                    {
                      t = make_lang_type (TREE_CODE (t));
                      pushtag (TYPE_IDENTIFIER ($<ttype>0), t, 0);
                      $<ttype>0 = t;
                    }
		  pushclass (t, 0);
		  TYPE_BEING_DEFINED (t) = 1;
		  /* Reset the interface data, at the earliest possible
		     moment, as it might have been set via a class foo;
		     before.  */
		  /* Don't change signatures.  */
		  if (! IS_SIGNATURE (t))
		    {
		      extern tree pending_vtables;
		      int needs_writing;
		      tree name = TYPE_IDENTIFIER (t);

		      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
		      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);

		      /* Record how to set the access of this class's
			 virtual functions.  If write_virtuals == 2 or 3, then
			 inline virtuals are ``extern inline''.  */
		      switch (write_virtuals)
			{
			case 0:
			case 1:
			  needs_writing = 1;
			  break;
			case 2:
			  needs_writing = !! value_member (name, pending_vtables);
			  break;
			case 3:
			  needs_writing = ! CLASSTYPE_INTERFACE_ONLY (t)
			    && CLASSTYPE_INTERFACE_KNOWN (t);
			  break;
			default:
			  needs_writing = 0;
			}
		      CLASSTYPE_VTABLE_NEEDS_WRITING (t) = needs_writing;
		    }
#if 0
		  t = TYPE_IDENTIFIER ($<ttype>0);
		  if (t && IDENTIFIER_TEMPLATE (t))
		    overload_template_name (t, 1);
#endif
		}
	;

opt.component_decl_list:
	/* empty */
		{ $$ = NULL_TREE; }
	| component_decl_list
		{
		  if (current_aggr == signature_type_node)
		    $$ = build_tree_list ((tree) access_public, $$);
		  else
		    $$ = build_tree_list ((tree) access_default, $$);
		}
	| opt.component_decl_list VISSPEC ':' component_decl_list
		{
		  tree visspec = (tree) $2;

		  if (current_aggr == signature_type_node)
		    {
		      error ("access specifier not allowed in signature");
		      visspec = (tree) access_public;
		    }
		  $$ = chainon ($$, build_tree_list (visspec, $4));
		}
	| opt.component_decl_list VISSPEC ':'
		{
		  if (current_aggr == signature_type_node)
		    error ("access specifier not allowed in signature");
		}
	;

/* Note: we no longer warn about the semicolon after a component_decl_list.
   ARM $9.2 says that the semicolon is optional, and therefore allowed.  */
component_decl_list:
	  component_decl
		{ if ($$ == void_type_node) $$ = NULL_TREE; 
		}
	| component_decl_list component_decl
		{ /* In pushdecl, we created a reverse list of names
		     in this binding level.  Make sure that the chain
		     of what we're trying to add isn't the item itself
		     (which can happen with what pushdecl's doing).  */
		  if ($2 != NULL_TREE && $2 != void_type_node)
		    {
		      if (TREE_CHAIN ($2) != $$)
			$$ = chainon ($$, $2);
		      else
			$$ = $2;
		    }
		}
	| component_decl_list ';'
	;

component_decl:
	  component_decl_1 ';'
	| component_decl_1 '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0); }
	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 '{' /* nodecls compstmt */
		{ $$ = finish_method ($$); }
	;

component_decl_1:
	/* Do not add a "typed_declspecs declarator" rule here for
	   speed; we need to call grok_x_components for enums, so the
	   speedup would be insignificant.  */
	  typed_declspecs components
		{
		  $$ = grok_x_components ($$, $2);
		}
	| declmods notype_components
		{ 
		  $$ = grok_x_components ($$, $2);
		}
	| notype_declarator maybe_raises maybeasm maybe_attribute
		{ $$ = grokfield ($$, NULL_TREE, $2, NULL_TREE, $3);
		  cplus_decl_attributes ($$, $4); }
	| ':' expr_no_commas
		{ $$ = grokbitfield (NULL_TREE, NULL_TREE, $2); }
	| error
		{ $$ = NULL_TREE; }

	/* These rules introduce a reduce/reduce conflict; in
		typedef int foo, bar;
		class A {
		  foo (bar);
		};
	   should "A::foo" be declared as a function or "A::bar" as a data
	   member? In other words, is "bar" an after_type_declarator or a
	   parmlist? */
	| typed_declspecs '(' parmlist ')' type_quals
		{ $$ = build_parse_node (CALL_EXPR, TREE_VALUE ($1),
					 $3, $5);
		  $$ = grokfield ($$, TREE_CHAIN ($1), NULL_TREE, NULL_TREE,
				  NULL_TREE); }
	| typed_declspecs LEFT_RIGHT type_quals
		{ $$ = build_parse_node (CALL_EXPR, TREE_VALUE ($1),
					 empty_parms (), $3);
		  $$ = grokfield ($$, TREE_CHAIN ($1), NULL_TREE, NULL_TREE,
				  NULL_TREE); }
	;

/* The case of exactly one component is handled directly by component_decl. */
components:
	  /* empty: possibly anonymous */
		{ $$ = NULL_TREE; }
	| component_declarator0
	| components ',' component_declarator
		{
		  /* In this context, void_type_node encodes
		     friends.  They have been recorded elsewhere.  */
		  if ($$ == void_type_node)
		    $$ = $3;
		  else
		    $$ = chainon ($$, $3);
		}
	;

notype_components:
	  /* empty: possibly anonymous */
		{ $$ = NULL_TREE; }
	| notype_component_declarator0
	| notype_components ',' notype_component_declarator
		{
		  /* In this context, void_type_node encodes
		     friends.  They have been recorded elsewhere.  */
		  if ($$ == void_type_node)
		    $$ = $3;
		  else
		    $$ = chainon ($$, $3);
		}
	;

component_declarator0:
	  after_type_component_declarator0
	| notype_component_declarator0
	;

component_declarator:
	  after_type_component_declarator
	| notype_component_declarator
	;

after_type_component_declarator0:
	  after_type_declarator maybe_raises maybeasm maybe_attribute
		{ current_declspecs = $<ttype>0;
		  $$ = grokfield ($$, current_declspecs, $2, NULL_TREE, $3);
		  cplus_decl_attributes ($$, $4); }
	| after_type_declarator maybe_raises maybeasm maybe_attribute '=' init
		{ current_declspecs = $<ttype>0;
		  $$ = grokfield ($$, current_declspecs, $2, $6, $3);
		  cplus_decl_attributes ($$, $4); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4); }
	;

notype_component_declarator0:
	  notype_declarator maybe_raises maybeasm maybe_attribute
		{ current_declspecs = $<ttype>0;
		  $$ = grokfield ($$, current_declspecs, $2, NULL_TREE, $3);
		  cplus_decl_attributes ($$, $4); }
	| notype_declarator maybe_raises maybeasm maybe_attribute '=' init
		{ current_declspecs = $<ttype>0;
		  $$ = grokfield ($$, current_declspecs, $2, $6, $3);
		  cplus_decl_attributes ($$, $4); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4); }
	| ':' expr_no_commas maybe_attribute
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield (NULL_TREE, current_declspecs, $2);
		  cplus_decl_attributes ($$, $3); }
	;

after_type_component_declarator:
	  after_type_declarator maybe_raises maybeasm maybe_attribute
		{ $$ = grokfield ($$, current_declspecs, $2, NULL_TREE, $3);
		  cplus_decl_attributes ($$, $4); }
	| after_type_declarator maybe_raises maybeasm maybe_attribute '=' init
		{ $$ = grokfield ($$, current_declspecs, $2, $6, $3);
		  cplus_decl_attributes ($$, $4); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4); }
	;

notype_component_declarator:
	  notype_declarator maybe_raises maybeasm maybe_attribute
		{ $$ = grokfield ($$, current_declspecs, $2, NULL_TREE, $3);
		  cplus_decl_attributes ($$, $4); }
	| notype_declarator maybe_raises maybeasm maybe_attribute '=' init
		{ $$ = grokfield ($$, current_declspecs, $2, $6, $3);
		  cplus_decl_attributes ($$, $4); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield (NULL_TREE, current_declspecs, $2);
		  cplus_decl_attributes ($$, $3); }
	;

/* We chain the enumerators in reverse order.
   Because of the way enums are built, the order is
   insignificant.  Take advantage of this fact.  */

enumlist:
	  enumerator
	| enumlist ',' enumerator
		{ TREE_CHAIN ($3) = $$; $$ = $3; }
	;

enumerator:
	  identifier
		{ $$ = build_enumerator ($$, NULL_TREE); }
	| identifier '=' expr_no_commas
		{ $$ = build_enumerator ($$, $3); }
	;

/* ANSI new-type-id (5.3.4) */
new_type_id:
	  type_specifier_seq new_declarator
		{ $$ = build_decl_list ($$, $2); }
	| type_specifier_seq %prec EMPTY
		{ $$ = build_decl_list ($$, NULL_TREE); }
	/* GNU extension to allow arrays of arbitrary types with
	   non-constant dimension.  */
	| '(' type_id ')' '[' expr ']'
		{
		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids array dimensions with parenthesized type in new");
		  $$ = build_parse_node (ARRAY_REF, TREE_VALUE ($2), $5);
		  $$ = build_decl_list (TREE_PURPOSE ($2), $$);
		}
	;

type_quals:
	  /* empty */ %prec EMPTY
		{ $$ = NULL_TREE; }
	| type_quals TYPE_QUAL
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	;

nonempty_type_quals:
	  TYPE_QUAL
		{ $$ = IDENTIFIER_AS_LIST ($$); }
	| nonempty_type_quals TYPE_QUAL
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are preferred.  */

/* An expression which will not live on the momentary obstack.  */
nonmomentary_expr:
	{ $<itype>$ = suspend_momentary (); } expr
	{ resume_momentary ((int) $<itype>1); $$ = $2; }
	;

/* A declarator that is allowed only after an explicit typespec.  */
/* may all be followed by prec '.' */
after_type_declarator:
	  '*' nonempty_type_quals after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' nonempty_type_quals after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	| '*' after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem type_quals after_type_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_after_type_declarator
	;

qualified_type_name:
	  type_name %prec EMPTY
		{
		  /* Remember that this name has been used in the class
		     definition, as per [class.scope0] */
		  if (current_class_type
		      && TYPE_BEING_DEFINED (current_class_type)
		      && ! IDENTIFIER_CLASS_VALUE ($$))
		    {
		      tree t = lookup_name ($$, -2);
		      if (t)
			pushdecl_class_level (t);
		    }
		}
	| nested_type
	;

nested_type:
	nested_name_specifier type_name %prec EMPTY
		{ $$ = $2; }
	;

direct_after_type_declarator:
	  direct_after_type_declarator '(' nonnull_exprlist ')' type_quals %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, $3, $5); }
	| direct_after_type_declarator '(' parmlist ')' type_quals %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, $3, $5); }
	| direct_after_type_declarator LEFT_RIGHT type_quals %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, empty_parms (), $3); }
	| direct_after_type_declarator '(' error ')' type_quals %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, NULL_TREE, NULL_TREE); }
	| direct_after_type_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_after_type_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	| '(' after_type_declarator ')'
		{ $$ = $2; }
	| nested_name_specifier type_name %prec EMPTY
		{ push_nested_class (TREE_TYPE ($$), 3);
		  $$ = build_parse_node (SCOPE_REF, $$, $2);
		  TREE_COMPLEXITY ($$) = current_class_depth; }
	| type_name %prec EMPTY
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  '*' nonempty_type_quals notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' nonempty_type_quals notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	| '*' notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem type_quals notype_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_notype_declarator
	;

complex_notype_declarator:
	  '*' nonempty_type_quals notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' nonempty_type_quals notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	| '*' complex_notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' complex_notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem type_quals notype_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| complex_direct_notype_declarator
	;

complex_direct_notype_declarator:
	  direct_notype_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, $3, $5); }
	| direct_notype_declarator '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, $3, $5); }
	| direct_notype_declarator LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, empty_parms (), $3); }
	| direct_notype_declarator '(' error ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, NULL_TREE, NULL_TREE); }
	| '(' expr_or_declarator ')'
		{ $$ = finish_decl_parsing ($2); }
	| '(' complex_notype_declarator ')'
		{ $$ = $2; }
	| direct_notype_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_notype_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	;

qualified_id:
	nested_name_specifier unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $$, $2); }
	;

notype_qualified_id:
	nested_name_specifier notype_unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $$, $2); }
	;

overqualified_id:
	  notype_qualified_id
	| global_scope notype_qualified_id
		{ $$ = $2; }
	;

functional_cast:
	  typespec '(' nonnull_exprlist ')'
		{ $$ = build_functional_cast ($$, $3); }
	| typespec '(' expr_or_declarator ')'
		{ $$ = reparse_decl_as_expr ($$, $3); }
	| typespec fcast_or_absdcl %prec EMPTY
		{ $$ = reparse_absdcl_as_expr ($$, $2); }
	;

type_name:
	  TYPENAME
	| template_type %prec EMPTY
	;

nested_name_specifier:
	  nested_name_specifier_1
	| nested_name_specifier nested_name_specifier_1
		{ $$ = $2; }
	;

/* Why the @#$%^& do type_name and notype_identifier need to be expanded
   inline here?!?  (jason) */
nested_name_specifier_1:
	  TYPENAME SCOPE
		{ got_scope = TREE_TYPE ($$); }
	| template_type SCOPE
		{ got_scope = TREE_TYPE ($$); }
/* 	These break 'const i;'
	| IDENTIFIER SCOPE
		{
		 failed_scope:
		  cp_error ("`%D' is not an aggregate typedef", 
			    lastiddecl ? lastiddecl : $$);
		  $$ = error_mark_node;
		}
	| PTYPENAME SCOPE
		{ goto failed_scope; } */
	;

complete_type_name:
	  qualified_type_name
	| global_scope qualified_type_name
		{ $$ = $2; }
	;

complex_type_name:
	  nested_type
	| global_scope qualified_type_name
		{ $$ = $2; }
	;

ptr_to_mem:
	  nested_name_specifier '*'
		{ got_scope = NULL_TREE; }
	| global_scope nested_name_specifier '*'
		{ $$ = $2; got_scope = NULL_TREE; }
	;

/* All uses of explicit global scope must go through this nonterminal so
   that got_scope will be set before yylex is called to get the next token. */
global_scope:
	  SCOPE
		{ got_scope = void_type_node; }
	;

/* ANSI new-declarator (5.3.4) */
new_declarator:
	  '*' type_quals new_declarator
		{ $$ = make_pointer_declarator ($2, $3); }
	| '*' type_quals  %prec EMPTY
		{ $$ = make_pointer_declarator ($2, NULL_TREE); }
	| '&' type_quals new_declarator %prec EMPTY
		{ $$ = make_reference_declarator ($2, $3); }
	| '&' type_quals %prec EMPTY
		{ $$ = make_reference_declarator ($2, NULL_TREE); }
	| ptr_to_mem type_quals %prec EMPTY
		{ tree arg = make_pointer_declarator ($2, NULL_TREE);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem type_quals new_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_new_declarator %prec EMPTY
	;

/* ANSI direct-new-declarator (5.3.4) */
direct_new_declarator:
	  '[' expr ']'
		{ $$ = build_parse_node (ARRAY_REF, NULL_TREE, $2); }
	| direct_new_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	;

/* ANSI abstract-declarator (8.1) */
absdcl:
	  '*' nonempty_type_quals absdcl
		{ $$ = make_pointer_declarator ($2, $3); }
	| '*' absdcl
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '*' nonempty_type_quals  %prec EMPTY
		{ $$ = make_pointer_declarator ($2, NULL_TREE); }
	| '*' %prec EMPTY
		{ $$ = make_pointer_declarator (NULL_TREE, NULL_TREE); }
	| '&' nonempty_type_quals absdcl
		{ $$ = make_reference_declarator ($2, $3); }
	| '&' absdcl
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| '&' nonempty_type_quals %prec EMPTY
		{ $$ = make_reference_declarator ($2, NULL_TREE); }
	| '&' %prec EMPTY
		{ $$ = make_reference_declarator (NULL_TREE, NULL_TREE); }
	| ptr_to_mem type_quals %prec EMPTY
		{ tree arg = make_pointer_declarator ($2, NULL_TREE);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem type_quals absdcl
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_abstract_declarator %prec EMPTY
	;

/* ANSI direct-abstract-declarator (8.1) */
direct_abstract_declarator:
	  '(' absdcl ')'
		{ $$ = $2; }
	  /* `(typedef)1' is `int'.  */
	| PAREN_STAR_PAREN
	| direct_abstract_declarator '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, $3, $5); }
	| direct_abstract_declarator LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $$, empty_parms (), $3); }
	| direct_abstract_declarator '[' nonmomentary_expr ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_abstract_declarator '[' ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	| '(' complex_parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, NULL_TREE, $2, $4); }
	| regcast_or_absdcl type_quals %prec '.'
		{ TREE_OPERAND ($$, 2) = $2; }
	| fcast_or_absdcl type_quals %prec '.'
		{ TREE_OPERAND ($$, 2) = $2; }
	| '[' nonmomentary_expr ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, NULL_TREE, $2); }
	| '[' ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, NULL_TREE, NULL_TREE); }
	;

/* For C++, decls and stmts can be intermixed, so we don't need to
   have a special rule that won't start parsing the stmt section
   until we have a stmt that parses without errors.  */

stmts:
	  stmt
	| errstmt
	| stmts stmt
	| stmts errstmt
	;

errstmt:  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

.pushlevel:  /* empty */
		{ emit_line_note (input_filename, lineno);
		  pushlevel (0);
		  clear_last_expr ();
		  push_momentary ();
		  expand_start_bindings (0); }
	;

/* Read zero or more forward-declarations for labels
   that nested functions can jump to.  */
maybe_label_decls:
	  /* empty */
	| label_decls
		{ if (flag_ansi)
		    pedwarn ("ANSI C++ forbids label declarations"); }
	;

label_decls:
	  label_decl
	| label_decls label_decl
	;

label_decl:
	  LABEL identifiers_or_typenames ';'
		{ tree link;
		  for (link = $2; link; link = TREE_CHAIN (link))
		    {
		      tree label = shadow_label (TREE_VALUE (link));
		      C_DECLARED_LABEL_FLAG (label) = 1;
		      declare_nonlocal_label (label);
		    }
		}
	;

/* This is the body of a function definition.
   It causes syntax errors to ignore to the next openbrace.  */
compstmt_or_error:
	  compstmt
		{}
	| error compstmt
	;

compstmt: '{' .pushlevel '}'
		{ expand_end_bindings (getdecls (), kept_level_p(), 1);
		  $$ = poplevel (kept_level_p (), 1, 0);
		  pop_momentary (); }
	| '{' .pushlevel maybe_label_decls stmts '}'
		{ expand_end_bindings (getdecls (), kept_level_p(), 1);
		  $$ = poplevel (kept_level_p (), 1, 0);
		  pop_momentary (); }
	| '{' .pushlevel maybe_label_decls stmts error '}'
		{ expand_end_bindings (getdecls (), kept_level_p(), 1);
		  $$ = poplevel (kept_level_p (), 0, 0);
		  pop_momentary (); }
	| '{' .pushlevel maybe_label_decls error '}'
		{ expand_end_bindings (getdecls (), kept_level_p(), 1);
		  $$ = poplevel (kept_level_p (), 0, 0);
		  pop_momentary (); }
	;

simple_if:
	  IF
		{ cond_stmt_keyword = "if"; }
	  .pushlevel paren_cond_or_null
		{ emit_line_note (input_filename, lineno);
		  expand_start_cond ($4, 0); }
	  implicitly_scoped_stmt
	;

implicitly_scoped_stmt:
	  compstmt
		{ finish_stmt (); }
	| .pushlevel simple_stmt
		{ expand_end_bindings (getdecls (), kept_level_p (), 1);
		  $$ = poplevel (kept_level_p (), 1, 0);
		  pop_momentary (); }
	;

stmt:
	  compstmt
		{ finish_stmt (); }
	| simple_stmt
	;

simple_stmt:
	  decl
		{ finish_stmt (); }
	| expr ';'
		{
		  tree expr = $1;
		  emit_line_note (input_filename, lineno);
		  /* Do default conversion if safe and possibly important,
		     in case within ({...}).  */
		  if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
		       && lvalue_p (expr))
		      || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
		    expr = default_conversion (expr);
		  cplus_expand_expr_stmt (expr);
		  clear_momentary ();
		  finish_stmt (); }
	| simple_if ELSE
		{ expand_start_else (); }
	  implicitly_scoped_stmt
		{ expand_end_cond ();
		  expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  finish_stmt (); }
	| simple_if %prec IF
		{ expand_end_cond ();
		  expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  finish_stmt (); }
	| WHILE
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop (1);
		  cond_stmt_keyword = "while"; }
	  .pushlevel paren_cond_or_null
		{ expand_exit_loop_if_false (0, $4); }
	  already_scoped_stmt
		{ expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  expand_end_loop ();
		  finish_stmt (); }
	| DO
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop_continue_elsewhere (1); }
	  implicitly_scoped_stmt WHILE
		{ expand_loop_continue_here ();
		  cond_stmt_keyword = "do"; }
	  paren_expr_or_null ';'
		{ emit_line_note (input_filename, lineno);
		  expand_exit_loop_if_false (0, $6);
		  expand_end_loop ();
		  clear_momentary ();
		  finish_stmt (); }
	| forhead.1
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  if ($1) cplus_expand_expr_stmt ($1);
		  expand_start_loop_continue_elsewhere (1); }
	  .pushlevel xcond ';'
		{ emit_line_note (input_filename, lineno);
		  if ($4) expand_exit_loop_if_false (0, $4); }
	  xexpr ')'
		/* Don't let the tree nodes for $7 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
		{ push_momentary (); }
	  already_scoped_stmt
		{ emit_line_note (input_filename, lineno);
		  expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  expand_loop_continue_here ();
		  if ($7) cplus_expand_expr_stmt ($7);
		  pop_momentary ();
		  expand_end_loop ();
		  finish_stmt (); }
	| forhead.2
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop_continue_elsewhere (1); }
	  .pushlevel xcond ';'
		{ emit_line_note (input_filename, lineno);
		  if ($4) expand_exit_loop_if_false (0, $4); }
	  xexpr ')'
		/* Don't let the tree nodes for $7 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
		{ push_momentary ();
		  $<itype>8 = lineno; }
	  already_scoped_stmt
		{ emit_line_note (input_filename, (int) $<itype>8);
		  expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  expand_loop_continue_here ();
		  if ($7) cplus_expand_expr_stmt ($7);
		  pop_momentary ();
		  expand_end_loop ();
		  finish_stmt ();
		}
	| SWITCH .pushlevel '(' condition ')'
		{ emit_line_note (input_filename, lineno);
		  c_expand_start_case ($4);
		  /* Don't let the tree nodes for $4 be discarded by
		     clear_momentary during the parsing of the next stmt.  */
		  push_momentary (); }
	  implicitly_scoped_stmt
		{ expand_end_case ($4);
		  pop_momentary ();
		  expand_end_bindings (getdecls (), kept_level_p (), 1);
		  poplevel (kept_level_p (), 1, 0);
		  pop_momentary ();
		  finish_stmt (); }
	| CASE expr_no_commas ':'
		{ register tree value = check_cp_case_value ($2);
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		  if (value != error_mark_node)
		    {
		      tree duplicate;
		      int success = pushcase (value, convert_and_check,
					      label, &duplicate);
		      if (success == 1)
			cp_error ("case label `%E' not within a switch statement", $2);
		      else if (success == 2)
			{
			  cp_error ("duplicate case value `%E'", $2);
			  cp_error_at ("`%E' previously used here", duplicate);
			}
		      else if (success == 3)
			warning ("case value out of range");
		      else if (success == 5)
			cp_error ("case label `%E' within scope of cleanup or variable array", $2);
		    }
		  define_case_label (label);
		}
	  stmt
	| CASE expr_no_commas ELLIPSIS expr_no_commas ':'
		{ register tree value1 = check_cp_case_value ($2);
		  register tree value2 = check_cp_case_value ($4);
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		  if (flag_ansi)
		    pedwarn ("ANSI C++ forbids range expressions in switch statement");
		  if (value1 != error_mark_node
		      && value2 != error_mark_node)
		    {
		      tree duplicate;
		      int success = pushcase_range (value1, value2,
						    convert_and_check, label,
						    &duplicate);
		      if (success == 1)
			error ("case label not within a switch statement");
		      else if (success == 2)
			{
			  error ("duplicate (or overlapping) case value");
			  error_with_decl (duplicate, "this is the first entry overlapping that value");
			}
		      else if (success == 3)
			warning ("case value out of range");
		      else if (success == 4)
			warning ("empty range specified");
		      else if (success == 5)
			error ("case label within scope of cleanup or variable array");
		    }
		  define_case_label (label);
		}
	  stmt
	| DEFAULT ':'
		{
		  tree duplicate;
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		  int success = pushcase (NULL_TREE, 0, label, &duplicate);
		  if (success == 1)
		    error ("default label not within a switch statement");
		  else if (success == 2)
		    {
		      error ("multiple default labels in one switch");
		      error_with_decl (duplicate, "this is the first default label");
		    }
		  define_case_label (NULL_TREE);
		}
	  stmt
	| BREAK ';'
		{ emit_line_note (input_filename, lineno);
		  if ( ! expand_exit_something ())
		    error ("break statement not within loop or switch"); }
	| CONTINUE ';'
		{ emit_line_note (input_filename, lineno);
		  if (! expand_continue_loop (0))
		    error ("continue statement not within a loop"); }
	| RETURN ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return (NULL_TREE); }
	| RETURN expr ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return ($2);
		  finish_stmt ();
		}
	| asm_keyword maybe_type_qual '(' string ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  expand_asm ($4);
		  finish_stmt ();
		}
	/* This is the case with just output operands.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, NULL_TREE, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with input operands as well.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with clobbered registers as well.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ':'
	  asm_operands ':' asm_clobbers ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, $10,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	| GOTO '*' expr ';'
		{ emit_line_note (input_filename, lineno);
		  expand_computed_goto ($3); }
	| GOTO identifier ';'
		{ tree decl;
		  emit_line_note (input_filename, lineno);
		  decl = lookup_label ($2);
		  TREE_USED (decl) = 1;
		  expand_goto (decl); }
	| label_colon stmt
		{ finish_stmt (); }
	| label_colon '}'
		{ error ("label must be followed by statement");
		  yyungetc ('}', 0);
		  finish_stmt (); }
	| ';'
		{ finish_stmt (); }
	| try_block
	;

try_block:
	  TRY '{' .pushlevel
		{ expand_start_try_stmts (); }
	  ansi_try_stmts
		{ expand_end_try_stmts ();
		  expand_start_all_catch (); }
	  handler_seq
		{ expand_end_all_catch (); }
	;

ansi_try_stmts:
	  '}'
		/* An empty try block is degenerate, but it's better to
		   do extra work here than to do all the special-case work
		   everywhere else.  */
		{ expand_end_bindings (0,1,1);
		  poplevel (2,0,0);
		}
	| stmts '}'
		{ expand_end_bindings (0,1,1);
		  poplevel (2,0,0);
		}
	| error '}'
		{ expand_end_bindings (0,1,1);
		  poplevel (2,0,0);
		}
	;

handler_seq:
	  /* empty */
	| handler_seq CATCH
		{ emit_line_note (input_filename, lineno); }
	  handler_args compstmt
		{ expand_end_catch_block (); }
	;

type_specifier_seq:
	  typed_typespecs %prec EMPTY
	| nonempty_type_quals %prec EMPTY
	;

handler_args:
	  '(' ELLIPSIS ')'
		{ expand_start_catch_block (NULL_TREE, NULL_TREE); }
	/* This doesn't allow reference parameters, the below does.
	| '(' type_specifier_seq absdcl ')'
		{ expand_start_catch_block ($2, $3); }
	| '(' type_specifier_seq ')'
		{ expand_start_catch_block ($2, NULL_TREE); }
	| '(' type_specifier_seq notype_declarator ')'
		{ expand_start_catch_block ($2, $3); }
	| '(' typed_typespecs after_type_declarator ')'
		{ expand_start_catch_block ($2, $3); }
	*/
	| '(' parm ')'
		{ expand_start_catch_block (TREE_PURPOSE ($2),
					    TREE_VALUE ($2)); }
	;

label_colon:
	  IDENTIFIER ':'
		{ tree label;
		do_label:
		  label = define_label (input_filename, lineno, $1);
		  if (label)
		    expand_label (label);
		}
	| PTYPENAME ':'
		{ goto do_label; }
	| TYPENAME ':'
		{ goto do_label; }
	;

forhead.1:
	  FOR '(' ';'
		{ $$ = NULL_TREE; }
	| FOR '(' expr ';'
		{ $$ = $3; }
	| FOR '(' '{' '}'
		{ $$ = NULL_TREE; }
	;

forhead.2:
	  FOR '(' decl
		{ $$ = 0; }
	| FOR '(' error ';'
		{ $$ = 0; }
	| FOR '(' '{' .pushlevel stmts '}'
		{ $$ = 1; }
	| FOR '(' '{' .pushlevel error '}'
		{ $$ = -1; }
	;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_type_qual:
	/* empty */
		{ emit_line_note (input_filename, lineno);
		  $$ = NULL_TREE; }
	| TYPE_QUAL
		{ emit_line_note (input_filename, lineno); }
	;

xexpr:
	/* empty */
		{ $$ = NULL_TREE; }
	| expr
	| error
		{ $$ = NULL_TREE; }
	;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
asm_operands: /* empty */
		{ $$ = NULL_TREE; }
	| nonnull_asm_operands
	;

nonnull_asm_operands:
	  asm_operand
	| nonnull_asm_operands ',' asm_operand
		{ $$ = chainon ($$, $3); }
	;

asm_operand:
	  STRING '(' expr ')'
		{ $$ = build_tree_list ($$, $3); }
	;

asm_clobbers:
	  STRING
		{ $$ = tree_cons (NULL_TREE, $$, NULL_TREE); }
	| asm_clobbers ',' STRING
		{ $$ = tree_cons (NULL_TREE, $3, $$); }
	;

/* This is what appears inside the parens in a function declarator.
   Its value is represented in the format that grokdeclarator expects.

   In C++, declaring a function with no parameters
   means that that function takes *no* parameters.  */

parmlist:  /* empty */
		{
		  if (strict_prototype)
		    $$ = void_list_node;
		  else
		    $$ = NULL_TREE;
		}
	| complex_parmlist
	| type_id
		{ $$ = tree_cons (NULL_TREE, $$, void_list_node);
		  TREE_PARMLIST ($$) = 1; }
	;

/* This nonterminal does not include the common sequence '(' type_id ')',
   as it is ambiguous and must be disambiguated elsewhere.  */
complex_parmlist:
	  parms
		{
		  $$ = chainon ($$, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		}
	| parms_comma ELLIPSIS
		{
		  TREE_PARMLIST ($$) = 1;
		}
	/* C++ allows an ellipsis without a separating ',' */
	| parms ELLIPSIS
		{
		  TREE_PARMLIST ($$) = 1;
		}
	| type_id ELLIPSIS
		{
		  $$ = build_tree_list (NULL_TREE, $$); 
		  TREE_PARMLIST ($$) = 1;
		}
	| ELLIPSIS
		{
		  /* ARM $8.2.5 has this as a boxed-off comment.  */
		  if (pedantic)
		    warning ("use of `...' without a first argument is non-portable");
		  $$ = NULL_TREE;
		}
	| TYPENAME_ELLIPSIS
		{
		  TREE_PARMLIST ($$) = 1;
		}
	| parms TYPENAME_ELLIPSIS
		{
		  TREE_PARMLIST ($$) = 1;
		}
	| type_id TYPENAME_ELLIPSIS
		{
		  $$ = build_tree_list (NULL_TREE, $$);
		  TREE_PARMLIST ($$) = 1;
		}
	| parms ':'
		{
		  /* This helps us recover from really nasty
		     parse errors, for example, a missing right
		     parenthesis.  */
		  yyerror ("possibly missing ')'");
		  $$ = chainon ($$, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		  yyungetc (':', 0);
		  yychar = ')';
		}
	| type_id ':'
		{
		  /* This helps us recover from really nasty
		     parse errors, for example, a missing right
		     parenthesis.  */
		  yyerror ("possibly missing ')'");
		  $$ = tree_cons (NULL_TREE, $$, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		  yyungetc (':', 0);
		  yychar = ')';
		}
	;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  named_parm
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| parm '=' init
		{ $$ = build_tree_list ($3, $$); }
	| parms_comma full_parm
		{ $$ = chainon ($$, $2); }
	| parms_comma bad_parm
		{ $$ = chainon ($$, build_tree_list (NULL_TREE, $2)); }
	| parms_comma bad_parm '=' init
		{ $$ = chainon ($$, build_tree_list ($4, $2)); }
	;

parms_comma:
	  parms ','
	| type_id ','
		{ $$ = build_tree_list (NULL_TREE, $$); }
	;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  The first four cases make up for 10%
   of the time spent parsing C++.  We cannot use them because
   of `int id[]' which won't get parsed properly.  */
named_parm:
/*
	  typed_declspecs dont_see_typename '*' IDENTIFIER
		{ $$ = build_tree_list ($$, build_parse_node (INDIRECT_REF, $4));
		  see_typename (); }
	| typed_declspecs dont_see_typename '&' IDENTIFIER
		{ $$ = build_tree_list ($$, build_parse_node (ADDR_EXPR, $4));
		  see_typename (); }
	| TYPENAME IDENTIFIER
		{ $$ = build_tree_list (get_decl_list ($$), $2);  }
	| TYPESPEC IDENTIFIER
		{ $$ = build_tree_list (get_decl_list ($$), $2); }
	| */
	/* Here we expand typed_declspecs inline to avoid mis-parsing of
	   TYPESPEC IDENTIFIER.  */
	  typed_declspecs1 declarator
		{ $$ = build_tree_list ($$, $2); }
	| typed_typespecs declarator
		{ $$ = build_tree_list ($$, $2); }
	| typespec declarator
		{ $$ = build_tree_list (get_decl_list ($$), $2); }
	| typed_declspecs1 absdcl
		{ $$ = build_tree_list ($$, $2); }
	| typed_declspecs1 %prec EMPTY
		{ $$ = build_tree_list ($$, NULL_TREE); }
	| declmods notype_declarator
		{ $$ = build_tree_list ($$, $2); }
	;

full_parm:
	  parm
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| parm '=' init
		{ $$ = build_tree_list ($3, $$); }
	;

parm:
	named_parm
	| type_id
	;

see_typename: %prec EMPTY
	{ see_typename (); }
	;

/* 
dont_see_typename: %prec EMPTY
	{ dont_see_typename (); }
	; 

try_for_typename:
        {
	  if ($<ttype>-1 == error_mark_node)
            $$ = 0;
          else
            {
              $$ = 1;
              pushclass ($<ttype>-1, 1);
            }
        }
	;
*/

bad_parm:
	  /* empty */ %prec EMPTY
		{
		  warning ("type specifier omitted for parameter");
		  $$ = build_tree_list (TREE_PURPOSE (TREE_VALUE ($<ttype>-1)), NULL_TREE);
		}
	| notype_declarator
		{
		  warning ("type specifier omitted for parameter");
		  $$ = build_tree_list (TREE_PURPOSE (TREE_VALUE ($<ttype>-1)), $$);
		}
	;

maybe_raises:
	  %prec EMPTY /* empty */
		{ $$ = NULL_TREE; }
	| THROW '(' ansi_raise_identifiers  ')' %prec EMPTY
		{ $$ = $3; }
	;

ansi_raise_identifier:
	  type_id
		{ $$ = build_decl_list (NULL_TREE, $$); }
	;

ansi_raise_identifiers:
	  ansi_raise_identifier
	| ansi_raise_identifiers ',' ansi_raise_identifier
		{
		  TREE_CHAIN ($3) = $$;
		  $$ = $3;
		}
	;

conversion_declarator:
	  /* empty */ %prec EMPTY
		{ $$ = NULL_TREE; }
	| '*' type_quals conversion_declarator
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' type_quals conversion_declarator
		{ $$ = make_reference_declarator ($2, $3); }
	| ptr_to_mem type_quals conversion_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	;

operator: OPERATOR
		{ got_scope = NULL_TREE; }
	;

operator_name:
	  operator '*'
		{ $$ = ansi_opname[MULT_EXPR]; }
	| operator '/'
		{ $$ = ansi_opname[TRUNC_DIV_EXPR]; }
	| operator '%'
		{ $$ = ansi_opname[TRUNC_MOD_EXPR]; }
	| operator '+'
		{ $$ = ansi_opname[PLUS_EXPR]; }
	| operator '-'
		{ $$ = ansi_opname[MINUS_EXPR]; }
	| operator '&'
		{ $$ = ansi_opname[BIT_AND_EXPR]; }
	| operator '|'
		{ $$ = ansi_opname[BIT_IOR_EXPR]; }
	| operator '^'
		{ $$ = ansi_opname[BIT_XOR_EXPR]; }
	| operator '~'
		{ $$ = ansi_opname[BIT_NOT_EXPR]; }
	| operator ','
		{ $$ = ansi_opname[COMPOUND_EXPR]; }
	| operator ARITHCOMPARE
		{ $$ = ansi_opname[$2]; }
	| operator '<'
		{ $$ = ansi_opname[LT_EXPR]; }
	| operator '>'
		{ $$ = ansi_opname[GT_EXPR]; }
	| operator EQCOMPARE
		{ $$ = ansi_opname[$2]; }
	| operator ASSIGN
		{ $$ = ansi_assopname[$2]; }
	| operator '='
		{ $$ = ansi_opname [MODIFY_EXPR]; }
	| operator LSHIFT
		{ $$ = ansi_opname[$2]; }
	| operator RSHIFT
		{ $$ = ansi_opname[$2]; }
	| operator PLUSPLUS
		{ $$ = ansi_opname[POSTINCREMENT_EXPR]; }
	| operator MINUSMINUS
		{ $$ = ansi_opname[PREDECREMENT_EXPR]; }
	| operator ANDAND
		{ $$ = ansi_opname[TRUTH_ANDIF_EXPR]; }
	| operator OROR
		{ $$ = ansi_opname[TRUTH_ORIF_EXPR]; }
	| operator '!'
		{ $$ = ansi_opname[TRUTH_NOT_EXPR]; }
	| operator '?' ':'
		{ $$ = ansi_opname[COND_EXPR]; }
	| operator MIN_MAX
		{ $$ = ansi_opname[$2]; }
	| operator POINTSAT  %prec EMPTY
		{ $$ = ansi_opname[COMPONENT_REF]; }
	| operator POINTSAT_STAR  %prec EMPTY
		{ $$ = ansi_opname[MEMBER_REF]; }
	| operator LEFT_RIGHT
		{ $$ = ansi_opname[CALL_EXPR]; }
	| operator '[' ']'
		{ $$ = ansi_opname[ARRAY_REF]; }
	| operator NEW %prec EMPTY
		{ $$ = ansi_opname[NEW_EXPR]; }
	| operator DELETE %prec EMPTY
		{ $$ = ansi_opname[DELETE_EXPR]; }
	| operator NEW '[' ']'
		{ $$ = ansi_opname[VEC_NEW_EXPR]; }
	| operator DELETE '[' ']'
		{ $$ = ansi_opname[VEC_DELETE_EXPR]; }
	/* Names here should be looked up in class scope ALSO.  */
	| operator type_specifier_seq conversion_declarator
		{ $$ = grokoptypename ($2, $3); }
	| operator error
		{ $$ = ansi_opname[ERROR_MARK]; }
	;

%%

#ifdef SPEW_DEBUG
const char *
debug_yytranslate (value)
    int value;
{
  return yytname[YYTRANSLATE (value)];
}

#endif
