/* YACC parser for C++ syntax.
   Copyright (C) 1988, 89, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Note: Bison automatically applies a default action of "$$ = $1" for
   all derivations; this is applied before the explicit action, if one
   is given.  Keep this in mind when reading the actions.  */

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
#include "output.h"
#include "except.h"

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
extern tree last_tree;

/* FSF LOCAL dje prefix attributes */
extern tree strip_attrs		PROTO((tree));
/* END FSF LOCAL */

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

#define OP0(NODE) (TREE_OPERAND (NODE, 0))
#define OP1(NODE) (TREE_OPERAND (NODE, 1))

/* Contains the statement keyword (if/while/do) to include in an
   error message if the user supplies an empty conditional expression.  */
static char *cond_stmt_keyword;

static tree empty_parms PROTO((void));

/* Nonzero if we have an `extern "C"' acting as an extern specifier.  */
int have_extern_spec;
int used_extern_spec;

/* Cons up an empty parameter list.  */
#ifdef __GNUC__
__inline
#endif
static tree
empty_parms ()
{
  tree parms;

  if (strict_prototype
      || current_class_type != NULL)
    parms = void_list_node;
  else
    parms = NULL_TREE;
  return parms;
}
%}

%start program

%union {long itype; tree ttype; char *strtype; enum tree_code code; flagged_type_tree ftype; }

/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME
%token SELFNAME

/* A template function.  */
%token PFUNCNAME

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC

/* Reserved words that qualify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token CV_QUALIFIER

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else.  */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM_KEYWORD GCC_ASM_KEYWORD TYPEOF ALIGNOF
%token SIGOF
%token ATTRIBUTE EXTENSION LABEL
%token REALPART IMAGPART

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <ttype> VISSPEC
%token DELETE NEW THIS OPERATOR CXX_TRUE CXX_FALSE
%token NAMESPACE TYPENAME_KEYWORD USING
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

%left IDENTIFIER PFUNCNAME TYPENAME SELFNAME PTYPENAME SCSPEC TYPESPEC CV_QUALIFIER ENUM AGGR ELLIPSIS TYPEOF SIGOF OPERATOR NSNAME TYPENAME_KEYWORD

%left '{' ',' ';'

%nonassoc THROW
%right <code> ':'
%right <code> ASSIGN '='
%right <code> '?'
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
%nonassoc NEW DELETE TRY CATCH

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> PFUNCNAME
%type <ttype> paren_expr_or_null nontrivial_exprlist SELFNAME
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> reserved_declspecs boolean.literal
%type <ttype> reserved_typespecquals
%type <ttype> declmods 
%type <ttype> SCSPEC TYPESPEC CV_QUALIFIER maybe_cv_qualifier
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm maybe_init defarg defarg1
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attributes attribute attribute_list attrib
%type <ttype> any_word

%type <ttype> compstmt implicitly_scoped_stmt

%type <ttype> declarator notype_declarator after_type_declarator
%type <ttype> direct_notype_declarator direct_after_type_declarator

%type <ttype> opt.component_decl_list component_decl_list
%type <ttype> component_decl component_decl_1 components notype_components
%type <ttype> component_declarator component_declarator0 self_reference
%type <ttype> notype_component_declarator notype_component_declarator0
%type <ttype> after_type_component_declarator after_type_component_declarator0
%type <ttype> enumlist enumerator
%type <ttype> absdcl cv_qualifiers
%type <ttype> direct_abstract_declarator conversion_declarator
%type <ttype> new_declarator direct_new_declarator
%type <ttype> xexpr parmlist parms bad_parm 
%type <ttype> identifiers_or_typenames
%type <ttype> fcast_or_absdcl regcast_or_absdcl
%type <ttype> expr_or_declarator complex_notype_declarator
%type <ttype> notype_unqualified_id unqualified_id qualified_id
%type <ttype> template_id object_template_id notype_template_declarator
%type <ttype> overqualified_id notype_qualified_id any_id
%type <ttype> complex_direct_notype_declarator functional_cast
%type <ttype> complex_parmlist parms_comma

%type <ftype> type_id new_type_id typed_typespecs typespec typed_declspecs
%type <ftype> typed_declspecs1 type_specifier_seq nonempty_cv_qualifiers
%type <ftype> structsp typespecqual_reserved parm named_parm full_parm

/* C++ extensions */
%token <ttype> TYPENAME_ELLIPSIS PTYPENAME
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL DEFARG DEFARG_MARKER
%type <ttype> fn.def1 /* Not really! */ component_constructor_declarator
%type <ttype> fn.def2 return_id fn.defpen constructor_declarator
%type <itype> ctor_initializer_opt
%type <ttype> named_class_head named_class_head_sans_basetype
%type <ttype> named_complex_class_head_sans_basetype
%type <ttype> unnamed_class_head
%type <ttype> class_head base_class_list
%type <ttype> base_class_access_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> exception_specification_opt ansi_raise_identifier ansi_raise_identifiers
%type <ttype> operator_name
%type <ttype> object aggr
%type <itype> new delete
/* %type <ttype> primary_no_id */
%type <ttype> nonmomentary_expr maybe_parmlist
%type <itype> initdcl0 notype_initdcl0 member_init_list
%type <ttype> template_header template_parm_list template_parm
%type <ttype> template_type_parm
%type <code>  template_close_bracket
%type <ttype> template_type template_arg_list template_arg
%type <ttype> condition xcond paren_cond_or_null
%type <ttype> type_name nested_name_specifier nested_type ptr_to_mem
%type <ttype> complete_type_name notype_identifier nonnested_type
%type <ttype> complex_type_name nested_name_specifier_1
%type <itype> nomods_initdecls nomods_initdcl0
%type <ttype> new_initializer new_placement
%type <ttype> using_decl .poplevel
%type <ttype> typename_sub typename_sub0 typename_sub1 typename_sub2
/* in order to recognize aggr tags as defining and thus shadowing.  */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> named_class_head_sans_basetype_defn
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN

%type <ttype> self_template_type

%token NSNAME
%type <ttype> NSNAME

/* Used in lex.c for parsing pragmas.  */
%token END_OF_LINE

/* lex.c and pt.c depend on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT

%{
/* List of types and structure classes of the current declaration.  */
static tree current_declspecs = NULL_TREE;
/* List of prefix attributes in effect.
   Prefix attributes are parsed by the reserved_declspecs and declmods
   rules.  They create a list that contains *both* declspecs and attrs.  */
/* ??? It is not clear yet that all cases where an attribute can now appear in
   a declspec list have been updated.  */
static tree prefix_attributes = NULL_TREE;

/* When defining an aggregate, this is the most recent one being defined.  */
static tree current_aggr;

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
extern void yyprint			PROTO((FILE *, int, YYSTYPE));
extern tree combine_strings		PROTO((tree));
%}

%%
program:
	  /* empty */
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
 can find a valid list of type and sc specs in $0.  */

extdefs:
		{ $<ttype>$ = NULL_TREE; }
	  lang_extdef
		{ $<ttype>$ = NULL_TREE; }
	| extdefs lang_extdef
		{ $<ttype>$ = NULL_TREE; }
	;

extdefs_opt:
	  extdefs
	| /* empty */
	;

.hush_warning:
		{ have_extern_spec = 1;
		  used_extern_spec = 0;
		  $<ttype>$ = NULL_TREE; }
	;
.warning_ok:
		{ have_extern_spec = 0; }
	;

extension:
	EXTENSION
		{ $<itype>$ = pedantic;
		  pedantic = 0; }
	;

asm_keyword:
	  ASM_KEYWORD
	| GCC_ASM_KEYWORD
	;

lang_extdef:
		{ if (pending_lang_change) do_pending_lang_change(); }
	  extdef
		{ if (! toplevel_bindings_p () && ! pseudo_global_level_p())
		  pop_everything (); }
	;

extdef:
	  fndef eat_saved_input
		{ if (pending_inlines) do_pending_inlines (); }
	| datadef
		{ if (pending_inlines) do_pending_inlines (); }
	| template_def
		{ if (pending_inlines) do_pending_inlines (); }
	| asm_keyword '(' string ')' ';'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3);
		  assemble_asm ($3); }
	| extern_lang_string '{' extdefs_opt '}'
		{ pop_lang_context (); }
	| extern_lang_string .hush_warning fndef .warning_ok eat_saved_input
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	| extern_lang_string .hush_warning datadef .warning_ok
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	| NAMESPACE identifier '{'
		{ push_namespace ($2); }
	  extdefs_opt '}'
		{ pop_namespace (); }
	| NAMESPACE '{'
		{ push_namespace (NULL_TREE); }
	  extdefs_opt '}'
		{ pop_namespace (); }
	| NAMESPACE identifier '=' any_id ';'
		{ do_namespace_alias ($2, $4); }
	| using_decl ';'
		{ do_toplevel_using_decl ($1); }
	| USING NAMESPACE any_id ';'
		{
		  if (TREE_CODE ($3) == IDENTIFIER_NODE)
		    $3 = lastiddecl;
		  do_using_directive ($3);
		}
	| extension extdef
		{ pedantic = $<itype>1; }
	;

using_decl:
	  USING qualified_id
		{ $$ = $2; }
	| USING global_scope qualified_id
		{ $$ = $3; }
	| USING global_scope unqualified_id
		{ $$ = $3; }
	;

any_id:
	  unqualified_id
	| qualified_id
	| global_scope qualified_id
		{ $$ = $2; }
	| global_scope unqualified_id
		{ $$ = $2; }
	;

extern_lang_string:
	EXTERN_LANG_STRING
		{ push_lang_context ($1); }
	| extern_lang_string EXTERN_LANG_STRING
		{ if (current_lang_name != $2)
		    cp_error ("use of linkage spec `%D' is different from previous spec `%D'", $2, current_lang_name);
		  pop_lang_context (); push_lang_context ($2); }
	;

template_header:
	  TEMPLATE '<'
		{ begin_template_parm_list (); }
	  template_parm_list '>'
		{ $$ = end_template_parm_list ($4); }
	| TEMPLATE '<' '>'
                { begin_specialization(); 
		  $$ = NULL_TREE; }
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
		    {
		      pedwarn ("template type parameters must use the keyword `class'");
		      TREE_PURPOSE ($$) = class_type_node;
		    }
		}
	| aggr identifier
		{ $$ = build_tree_list ($1, $2); goto ttpa; }
	| TYPENAME_KEYWORD
		{ $$ = build_tree_list (class_type_node, NULL_TREE); }
	| TYPENAME_KEYWORD identifier
		{ $$ = build_tree_list (class_type_node, $2); }
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
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| template_type_parm '=' type_id
		{ $$ = build_tree_list (groktypename ($3.t), $1); }
	| parm
		{ $$ = build_tree_list (NULL_TREE, $1.t); }
	| parm '=' expr_no_commas  %prec ARITHCOMPARE
		{ $$ = build_tree_list ($3, $1.t); }
	;

template_def:
	  template_header
	  extdef
                { 
                  if ($1) 
                    end_template_decl (); 
		  else
		    end_specialization ();
		}
	| template_header
	  error  %prec EMPTY
		{ 
                  if ($1) 
                    end_template_decl ();
		  else
		    end_specialization (); 
                }
	;

datadef:
	  nomods_initdecls ';'
		{}
	| declmods notype_initdecls ';'
		{}
	| typed_declspecs initdecls ';'
		{
		  note_list_got_semicolon ($1.t);
		}
        | declmods ';'
		{ pedwarn ("empty declaration"); }
	| explicit_instantiation ';'
	| typed_declspecs ';'
		{
		  tree t, attrs;
		  split_specs_attrs ($1.t, &t, &attrs);
		  shadow_tag (t);
		  note_list_got_semicolon ($1.t);
		}
	| error ';'
	| error '}'
	| ';'
	;

ctor_initializer_opt:
	  nodecls
		{ $$ = 0; }
	| base_init
		{ $$ = 1; }
	;

maybe_return_init:
	  /* empty */
	| return_init
	| return_init ';'
	;

eat_saved_input:
	  /* empty */
	| END_OF_SAVED_INPUT
	;

fndef:
	  fn.def1 maybe_return_init ctor_initializer_opt compstmt_or_error
		{ finish_function (lineno, (int)$3, 0); }
	| fn.def1 maybe_return_init function_try_block
		{ }
	| fn.def1 maybe_return_init error
		{ }
	;

constructor_declarator:
	  nested_name_specifier SELFNAME '(' 
		{
		  $$ = build_parse_node (SCOPE_REF, $1, $2);
		  if ($1 != current_class_type)
		    {
		      push_nested_class ($1, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
	  parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>4, $5, $7, $8); }
	| nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
		{
		  $$ = build_parse_node (SCOPE_REF, $1, $2);
		  if ($1 != current_class_type)
		    {
		      push_nested_class ($1, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		  $$ = make_call_declarator ($$, empty_parms (), $4, $5);
		}
	| global_scope nested_name_specifier SELFNAME '(' 
		{
		  $$ = build_parse_node (SCOPE_REF, $2, $3);
		  if ($2 != current_class_type)
		    {
		      push_nested_class ($2, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
	 parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>5, $6, $8, $9); }
	| global_scope nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
		{
		  $$ = build_parse_node (SCOPE_REF, $2, $3);
		  if ($2 != current_class_type)
		    {
		      push_nested_class ($2, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		  $$ = make_call_declarator ($$, empty_parms (), $5, $6);
		}
	| nested_name_specifier self_template_type '(' 
		{
		  $$ = build_parse_node (SCOPE_REF, $1, $2);
		  if ($1 != current_class_type)
		    {
		      push_nested_class ($1, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
	  parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>4, $5, $7, $8); }
	| nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{
		  $$ = build_parse_node (SCOPE_REF, $1, $2);
		  if ($1 != current_class_type)
		    {
		      push_nested_class ($1, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		  $$ = make_call_declarator ($$, empty_parms (), $4, $5);
		}
	| global_scope nested_name_specifier self_template_type '(' 
		{
		  $$ = build_parse_node (SCOPE_REF, $2, $3);
		  if ($2 != current_class_type)
		    {
		      push_nested_class ($2, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
	 parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>5, $6, $8, $9); }
	| global_scope nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{
		  $$ = build_parse_node (SCOPE_REF, $2, $3);
		  if ($2 != current_class_type)
		    {
		      push_nested_class ($2, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		  $$ = make_call_declarator ($$, empty_parms (), $5, $6);
		}
	;

fn.def1:
	  typed_declspecs declarator
		{ tree specs, attrs;
		  split_specs_attrs ($1.t, &specs, &attrs);
		  if (! start_function (specs, $2, attrs, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| declmods notype_declarator
		{ tree specs, attrs;
		  split_specs_attrs ($1, &specs, &attrs);
		  if (! start_function (specs, $2, attrs, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| notype_declarator
		{ if (! start_function (NULL_TREE, $$, NULL_TREE, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| declmods constructor_declarator
		{ tree specs, attrs;
		  split_specs_attrs ($1, &specs, &attrs);
		  if (! start_function (specs, $2, attrs, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	| constructor_declarator
		{ if (! start_function (NULL_TREE, $$, NULL_TREE, 0))
		    YYERROR1;
		  reinit_parse_for_function ();
		  $$ = NULL_TREE; }
	;

component_constructor_declarator:
	  SELFNAME '(' parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, $3, $5, $6); }
	| SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, empty_parms (), $3, $4); }
	| self_template_type '(' parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, $3, $5, $6); }
	| self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, empty_parms (), $3, $4); }
	;

/* more C++ complexity.  See component_decl for a comment on the
   reduce/reduce conflict introduced by these rules.  */
fn.def2:
	  declmods component_constructor_declarator
		{ tree specs = strip_attrs ($1);
		  $$ = start_method (specs, $2);
		 rest_of_mdef:
		  if (! $$)
		    YYERROR1;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| component_constructor_declarator
		{ $$ = start_method (NULL_TREE, $1); goto rest_of_mdef; }
	| typed_declspecs declarator
		{ tree specs = strip_attrs ($1.t);
		  $$ = start_method (specs, $2); goto rest_of_mdef; }
	| declmods notype_declarator
		{ tree specs = strip_attrs ($1);
		  $$ = start_method (specs, $2); goto rest_of_mdef; }
	| notype_declarator
		{ $$ = start_method (NULL_TREE, $$); goto rest_of_mdef; }
	| declmods constructor_declarator
		{ tree specs = strip_attrs ($1);
		  $$ = start_method (specs, $2); goto rest_of_mdef; }
	| constructor_declarator
		{ $$ = start_method (NULL_TREE, $$); goto rest_of_mdef; }
        | template_header fn.def2 
                { 
                  if ($1)
                    end_template_decl (); 
		  else
		    end_specialization ();

		  if ($2 && DECL_TEMPLATE_INFO ($2)
		      && !DECL_TEMPLATE_SPECIALIZATION ($2))
		    {
		      $$ = DECL_TI_TEMPLATE ($2); 
		      check_member_template ($$);
		    }
		  else if ($2)
		    $$ = $2;
		  else 
		    {
		      cp_error("invalid member template declaration");
		      $$ = NULL_TREE;
		    }
		}
	;

return_id:
	  RETURN IDENTIFIER
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  $$ = $2;
		}
	;

return_init:
	  return_id maybe_init
		{ store_return_init ($<ttype>$, $2); }
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
		     pair of curly braces of a function.  These are needed
		     for correct operation of dwarfout.c.  */
		  keep_next_level ();
		}
	;

.set_base_init:
	  /* empty */
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();

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

member_init:
	  '(' nonnull_exprlist ')'
		{
		  if (current_class_name)
		    pedwarn ("anachronistic old style base class initializer");
		  expand_member_init (current_class_ref, NULL_TREE, $2);
		}
	| LEFT_RIGHT
		{
		  if (current_class_name)
		    pedwarn ("anachronistic old style base class initializer");
		  expand_member_init (current_class_ref, NULL_TREE, void_type_node);
		}
	| notype_identifier '(' nonnull_exprlist ')'
		{ expand_member_init (current_class_ref, $1, $3); }
	| notype_identifier LEFT_RIGHT
		{ expand_member_init (current_class_ref, $1, void_type_node); }
	| nonnested_type '(' nonnull_exprlist ')'
		{ expand_member_init (current_class_ref, $1, $3); }
	| nonnested_type LEFT_RIGHT
		{ expand_member_init (current_class_ref, $1, void_type_node); }
	| typename_sub '(' nonnull_exprlist ')'
		{ expand_member_init (current_class_ref, TYPE_MAIN_DECL ($1),
				      $3); }
	| typename_sub LEFT_RIGHT
		{ expand_member_init (current_class_ref, TYPE_MAIN_DECL ($1),
				      void_type_node); }
	;

identifier:
	  IDENTIFIER
	| TYPENAME
	| SELFNAME
	| PTYPENAME
	| NSNAME
	;

notype_identifier:
	  IDENTIFIER
	| PTYPENAME 
	| NSNAME  %prec EMPTY
	;

identifier_defn:
	  IDENTIFIER_DEFN
	| TYPENAME_DEFN
	| PTYPENAME_DEFN
	;

explicit_instantiation:
	  TEMPLATE typespec ';'
		{ do_type_instantiation ($2.t, NULL_TREE);
		  yyungetc (';', 1); }
	| TEMPLATE typed_declspecs declarator
		{ tree specs = strip_attrs ($2.t);
		  do_decl_instantiation (specs, $3, NULL_TREE); }
	| TEMPLATE notype_declarator
		{ do_decl_instantiation (NULL_TREE, $2, NULL_TREE); }
	| TEMPLATE constructor_declarator
		{ do_decl_instantiation (NULL_TREE, $2, NULL_TREE); }
	| SCSPEC TEMPLATE typespec ';'
		{ do_type_instantiation ($3.t, $1);
		  yyungetc (';', 1); }
	| SCSPEC TEMPLATE typed_declspecs declarator
		{ tree specs = strip_attrs ($3.t);
		  do_decl_instantiation (specs, $4, $1); }
	| SCSPEC TEMPLATE notype_declarator
		{ do_decl_instantiation (NULL_TREE, $3, $1); }
	| SCSPEC TEMPLATE constructor_declarator
		{ do_decl_instantiation (NULL_TREE, $3, $1); }
	;

/* The TYPENAME expansions are to deal with use of a template class name as
  a template within the class itself, where the template decl is hidden by
  a type decl.  Got all that?  */

template_type:
	  PTYPENAME '<' template_arg_list template_close_bracket
		{
		  $$ = lookup_template_class ($1, $3, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	| PTYPENAME '<' template_close_bracket
		{
		  $$ = lookup_template_class ($1, NULL_TREE, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	| TYPENAME  '<' template_arg_list template_close_bracket
		{
		  $$ = lookup_template_class ($1, $3, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	| TYPENAME '<' template_close_bracket
		{
		  $$ = lookup_template_class ($1, NULL_TREE, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	| self_template_type
	;

self_template_type:
	  SELFNAME  '<' template_arg_list template_close_bracket
		{
		  $$ = lookup_template_class ($1, $3, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	| SELFNAME '<' template_close_bracket
		{
		  $$ = lookup_template_class ($1, NULL_TREE, NULL_TREE);
		  if ($$ != error_mark_node)
		    $$ = TYPE_STUB_DECL ($$);
		}
	;

template_close_bracket:
	  '>'
	| RSHIFT 
		{
		  /* Handle `Class<Class<Type>>' without space in the `>>' */
		  pedwarn ("`>>' should be `> >' in template class name");
		  yyungetc ('>', 1);
		}
	;

template_arg_list:
	  template_arg
		{ $$ = build_tree_list (NULL_TREE, $$); }
	| template_arg_list ',' template_arg
		{ $$ = chainon ($$, build_tree_list (NULL_TREE, $3)); }
	;

template_arg:
	  type_id
		{ $$ = groktypename ($1.t); }
	| expr_no_commas  %prec ARITHCOMPARE
	;

unop:
	  '-'
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

expr:
	  nontrivial_exprlist
		{ $$ = build_x_compound_expr ($$); }
	| expr_no_commas
	;

paren_expr_or_null:
	LEFT_RIGHT
		{ error ("ANSI C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' expr ')'
		{ $$ = condition_conversion ($2); }
	;

paren_cond_or_null:
	LEFT_RIGHT
		{ error ("ANSI C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' condition ')'
		{ $$ = condition_conversion ($2); }
	;

xcond:
	  /* empty */
		{ $$ = NULL_TREE; }
	| condition
		{ $$ = condition_conversion ($$); }
	| error
		{ $$ = NULL_TREE; }
	;

condition:
	  type_specifier_seq declarator maybeasm maybe_attribute '='
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
		  current_declspecs = $1.t;
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>2, current_declspecs, 1);
		  cplus_decl_attributes ($<ttype>$, $4,
					 /*prefix_attributes*/ NULL_TREE);
		}
	  init
		{ 
		  cp_finish_decl ($<ttype>6, $7, $4, 1, LOOKUP_ONLYCONVERTING);
		  resume_momentary ($<itype>5);
		  $$ = $<ttype>6; 
		  if (TREE_CODE (TREE_TYPE ($$)) == ARRAY_TYPE)
		    cp_error ("definition of array `%#D' in condition", $$); 
		}
	| expr
	;

compstmtend:
	  '}'
	| maybe_label_decls stmts '}'
	| maybe_label_decls stmts error '}'
	| maybe_label_decls error '}'
	;

already_scoped_stmt:
	  '{'
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (COMPOUND_STMT, NULL_TREE);
		      COMPOUND_STMT_NO_SCOPE ($<ttype>$) = 1;
		      add_tree ($<ttype>$);
		    }
		}
	  compstmtend
		{ 
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  finish_stmt (); 
		}
	| simple_stmt
	;


nontrivial_exprlist:
	  expr_no_commas ',' expr_no_commas
		{ $$ = expr_tree_cons (NULL_TREE, $$, 
		                  build_expr_list (NULL_TREE, $3)); }
	| expr_no_commas ',' error
		{ $$ = expr_tree_cons (NULL_TREE, $$, 
		                  build_expr_list (NULL_TREE, error_mark_node)); }
	| nontrivial_exprlist ',' expr_no_commas
		{ chainon ($$, build_expr_list (NULL_TREE, $3)); }
	| nontrivial_exprlist ',' error
		{ chainon ($$, build_expr_list (NULL_TREE, error_mark_node)); }
	;

nonnull_exprlist:
	  expr_no_commas
		{ $$ = build_expr_list (NULL_TREE, $$); }
	| nontrivial_exprlist
	;

unary_expr:
	  primary  %prec UNARY
		{ $$ = $1; }
	/* __extension__ turns off -pedantic for following primary.  */
	| extension cast_expr  	  %prec UNARY
		{ $$ = $2;
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
		  if (pedantic)
		    pedwarn ("ANSI C++ forbids `&&'");
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
		{ $$ = expr_sizeof ($2); }
	| SIZEOF '(' type_id ')'  %prec HYPERUNARY
		{ $$ = c_sizeof (groktypename ($3.t)); }
	| ALIGNOF unary_expr  %prec UNARY
		{ $$ = grok_alignof ($2); }
	| ALIGNOF '(' type_id ')'  %prec HYPERUNARY
		{ $$ = c_alignof (groktypename ($3.t)); 
		  check_for_new_type ("alignof", $3); }

	/* The %prec EMPTY's here are required by the = init initializer
	   syntax extension; see below.  */
	| new new_type_id  %prec EMPTY
		{ $$ = build_new (NULL_TREE, $2.t, NULL_TREE, $1); 
		  check_for_new_type ("new", $2); }
	| new new_type_id new_initializer
		{ $$ = build_new (NULL_TREE, $2.t, $3, $1); 
		  check_for_new_type ("new", $2); }
	| new new_placement new_type_id  %prec EMPTY
		{ $$ = build_new ($2, $3.t, NULL_TREE, $1); 
		  check_for_new_type ("new", $3); }
	| new new_placement new_type_id new_initializer
		{ $$ = build_new ($2, $3.t, $4, $1); 
		  check_for_new_type ("new", $3); }
	| new '(' type_id ')'  %prec EMPTY
		{ $$ = build_new (NULL_TREE, groktypename($3.t),
				  NULL_TREE, $1); 
		  check_for_new_type ("new", $3); }
	| new '(' type_id ')' new_initializer
		{ $$ = build_new (NULL_TREE, groktypename($3.t), $5, $1); 
		  check_for_new_type ("new", $3); }
	| new new_placement '(' type_id ')'  %prec EMPTY
		{ $$ = build_new ($2, groktypename($4.t), NULL_TREE, $1); 
		  check_for_new_type ("new", $4); }
	| new new_placement '(' type_id ')' new_initializer
		{ $$ = build_new ($2, groktypename($4.t), $6, $1); 
		  check_for_new_type ("new", $4); }

	| delete cast_expr  %prec UNARY
		{ $$ = delete_sanity ($2, NULL_TREE, 0, $1); }
	| delete '[' ']' cast_expr  %prec UNARY
		{ $$ = delete_sanity ($4, NULL_TREE, 1, $1);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX; }
	| delete '[' expr ']' cast_expr  %prec UNARY
		{ $$ = delete_sanity ($5, $3, 2, $1);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX; }
	| REALPART cast_expr %prec UNARY
		{ $$ = build_x_unary_op (REALPART_EXPR, $2); }
	| IMAGPART cast_expr %prec UNARY
		{ $$ = build_x_unary_op (IMAGPART_EXPR, $2); }
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
		  cp_error ("`%T' is not a valid expression", $2.t);
		  $$ = error_mark_node;
		}
	/* GNU extension so people can use initializer lists.  Note that
	   this alters the meaning of `new int = 1', which was previously
	   syntactically valid but semantically invalid.  */
	| '=' init
		{
		  if (pedantic)
		    pedwarn ("ANSI C++ forbids initialization of new expression with `='");
		  $$ = $2;
		}
	;

/* This is necessary to postpone reduction of `int ((int)(int)(int))'.  */
regcast_or_absdcl:
	  '(' type_id ')'  %prec EMPTY
		{ $2.t = tree_cons (NULL_TREE, $2.t, void_list_node);
		  TREE_PARMLIST ($2.t) = 1;
		  $$ = make_call_declarator (NULL_TREE, $2.t, NULL_TREE, NULL_TREE);
		  check_for_new_type ("cast", $2); }
	| regcast_or_absdcl '(' type_id ')'  %prec EMPTY
		{ $3.t = tree_cons (NULL_TREE, $3.t, void_list_node);
		  TREE_PARMLIST ($3.t) = 1;
		  $$ = make_call_declarator ($$, $3.t, NULL_TREE, NULL_TREE);
		  check_for_new_type ("cast", $3); }
	;

cast_expr:
	  unary_expr
	| regcast_or_absdcl unary_expr  %prec UNARY
		{ $$ = reparse_absdcl_as_casts ($$, $2); }
	| regcast_or_absdcl '{' initlist maybecomma '}'  %prec UNARY
		{ 
		  tree init = build_nt (CONSTRUCTOR, NULL_TREE,
					nreverse ($3)); 
		  if (pedantic)
		    pedwarn ("ANSI C++ forbids constructor-expressions");
		  /* Indicate that this was a GNU C constructor expression.  */
		  TREE_HAS_CONSTRUCTOR (init) = 1;

		  $$ = reparse_absdcl_as_casts ($$, init);
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
		{ $$ = build_x_modify_expr ($$, NOP_EXPR, $3);
		  if ($$ != error_mark_node)
                    C_SET_EXP_ORIGINAL_CODE ($$, MODIFY_EXPR); }
	| expr_no_commas ASSIGN expr_no_commas
		{ $$ = build_x_modify_expr ($$, $2, $3); }
	| THROW
		{ $$ = build_throw (NULL_TREE); }
	| THROW expr_no_commas
		{ $$ = build_throw ($2); }
/* These extensions are not defined.  The second arg to build_m_component_ref
   is old, build_m_component_ref now does an implicit
   build_indirect_ref (x, NULL_PTR) on the second argument.
	| object '&' expr_no_commas  %prec UNARY
		{ $$ = build_m_component_ref ($$, build_x_unary_op (ADDR_EXPR, $3)); }
	| object unop expr_no_commas  %prec UNARY
		{ $$ = build_m_component_ref ($$, build_x_unary_op ($2, $3)); }
	| object '(' type_id ')' expr_no_commas  %prec UNARY
		{ tree type = groktypename ($3.t);
		  $$ = build_m_component_ref ($$, build_c_cast (type, $5)); }
	| object primary_no_id  %prec UNARY
		{ $$ = build_m_component_ref ($$, $2); }
*/
	;

notype_unqualified_id:
	  '~' see_typename identifier
		{ $$ = build_parse_node (BIT_NOT_EXPR, $3); }
        | template_id
	| operator_name
	| IDENTIFIER
	| PTYPENAME
	| NSNAME  %prec EMPTY
	;

template_id:
        PFUNCNAME '<' template_arg_list template_close_bracket 
                { $$ = lookup_template_function ($1, $3); }
        | PFUNCNAME '<' template_close_bracket
                { $$ = lookup_template_function ($1, NULL_TREE); }
        | operator_name '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function 
		    (do_identifier ($1, 1), $3); }
        | operator_name '<' template_close_bracket
                { $$ = lookup_template_function 
		    (do_identifier ($1, 1), NULL_TREE); }
	;

object_template_id:
        TEMPLATE identifier '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function ($2, $4); }
        | TEMPLATE PFUNCNAME '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function (DECL_NAME ($2), $4); }
        | TEMPLATE operator_name '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function (DECL_NAME ($2), $4); }
        ;

unqualified_id:
	  notype_unqualified_id
	| TYPENAME
	| SELFNAME
	;

expr_or_declarator:
	  notype_unqualified_id
	| '*' expr_or_declarator  %prec UNARY
		{ $$ = build_parse_node (INDIRECT_REF, $2); }
	| '&' expr_or_declarator  %prec UNARY
		{ $$ = build_parse_node (ADDR_EXPR, $2); }
	| '(' expr_or_declarator ')'
		{ $$ = $2; }
	;

notype_template_declarator:
	  IDENTIFIER '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function ($1, $3); }
	| NSNAME '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function ($1, $3); }
	;
		
direct_notype_declarator:
	  complex_direct_notype_declarator
	| notype_unqualified_id
	| notype_template_declarator
	| '(' expr_or_declarator ')'
		{ $$ = finish_decl_parsing ($2); }
	;

primary:
	  notype_unqualified_id
		{
		  if (TREE_CODE ($$) == BIT_NOT_EXPR)
		    $$ = build_x_unary_op (BIT_NOT_EXPR, TREE_OPERAND ($$, 0));
		  else if (TREE_CODE ($$) != TEMPLATE_ID_EXPR)
		    $$ = do_identifier ($$, 1);
		}		
	| CONSTANT
	| boolean.literal
	| string
		{
		  if (processing_template_decl)
		    push_obstacks (&permanent_obstack, &permanent_obstack);
		  $$ = combine_strings ($$);
		  if (processing_template_decl)
		    pop_obstacks ();
		}
	| '(' expr ')'
		{ char class;
		  $$ = $2;
		  class = TREE_CODE_CLASS (TREE_CODE ($$));
		  if (class == 'e' || class == '1'
		      || class == '2' || class == '<')
                    /* This inhibits warnings in truthvalue_conversion.  */
		    C_SET_EXP_ORIGINAL_CODE ($$, ERROR_MARK); }
	| '(' expr_or_declarator ')'
		{ char class;
		  $$ = reparse_decl_as_expr (NULL_TREE, $2);
		  class = TREE_CODE_CLASS (TREE_CODE ($$));
		  if (class == 'e' || class == '1'
		      || class == '2' || class == '<')
                    /* This inhibits warnings in truthvalue_conversion.  */
		    C_SET_EXP_ORIGINAL_CODE ($$, ERROR_MARK); }
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
		  if (pedantic)
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
                {
                  $$ = build_x_function_call ($1, $3, current_class_ref); 
                  if (TREE_CODE ($$) == CALL_EXPR
                      && TREE_TYPE ($$) != void_type_node)
	            $$ = require_complete_type ($$);
                }
	| primary LEFT_RIGHT
                {
		  $$ = build_x_function_call ($$, NULL_TREE, current_class_ref);
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
		{ if (current_class_ptr)
		    {
#ifdef WARNING_ABOUT_CCD
		      TREE_USED (current_class_ptr) = 1;
#endif
		      $$ = current_class_ptr;
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
	| CV_QUALIFIER '(' nonnull_exprlist ')'
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
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("dynamic_cast", $3);
		  $$ = build_dynamic_cast (type, $6); }
	| STATIC_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("static_cast", $3);
		  $$ = build_static_cast (type, $6); }
	| REINTERPRET_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("reinterpret_cast", $3);
		  $$ = build_reinterpret_cast (type, $6); }
	| CONST_CAST '<' type_id '>' '(' expr ')'
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("const_cast", $3);
		  $$ = build_const_cast (type, $6); }
	| TYPEID '(' expr ')'
		{ $$ = build_x_typeid ($3); }
	| TYPEID '(' type_id ')'
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("typeid", $3);
		  $$ = get_typeid (TYPE_MAIN_VARIANT (type)); }
	| global_scope IDENTIFIER
		{ $$ = do_scoped_id ($2, 1); }
	| global_scope operator_name
		{
		  got_scope = NULL_TREE;
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    $$ = do_scoped_id ($2, 1);
		  else
		    $$ = $2;
		}
	| overqualified_id  %prec HYPERUNARY
		{ $$ = build_offset_ref (OP0 ($$), OP1 ($$)); }
	| overqualified_id '(' nonnull_exprlist ')'
		{ if (processing_template_decl)
		    $$ = build_min_nt (CALL_EXPR, copy_to_permanent ($1), $3, NULL_TREE);
		  else
		    $$ = build_member_call (OP0 ($$), OP1 ($$), $3); }
	| overqualified_id LEFT_RIGHT
		{ if (processing_template_decl)
		    $$ = build_min_nt (CALL_EXPR, copy_to_permanent ($1), 
				       NULL_TREE, NULL_TREE);
		  else
		    $$ = build_member_call (OP0 ($$), OP1 ($$), NULL_TREE); }
        | object object_template_id %prec UNARY
                { 
		  $$ = build_x_component_ref ($$, $2, NULL_TREE, 1); 
		}
        | object object_template_id '(' nonnull_exprlist ')'
                {
		  $$ = build_method_call ($1, $2, $4, 
					  NULL_TREE, LOOKUP_NORMAL); 
                }
	| object object_template_id LEFT_RIGHT
                {
		  $$ = build_method_call ($1, $2, NULL_TREE,
					  NULL_TREE, LOOKUP_NORMAL); 
                }
	| object unqualified_id  %prec UNARY
		{ $$ = build_x_component_ref ($$, $2, NULL_TREE, 1); }
	| object overqualified_id  %prec UNARY
		{ if (processing_template_decl)
		    $$ = build_min_nt (COMPONENT_REF, $1, copy_to_permanent ($2));
		  else
		    $$ = build_object_ref ($$, OP0 ($2), OP1 ($2)); }
	| object unqualified_id '(' nonnull_exprlist ')'
		{
#if 0
		  /* This is a future direction of this code, but because
		     build_x_function_call cannot always undo what is done
		     in build_component_ref entirely yet, we cannot do this.  */
		  $$ = build_x_function_call (build_component_ref ($$, $2, NULL_TREE, 1), $4, current_class_ref);
		  if (TREE_CODE ($$) == CALL_EXPR
		      && TREE_TYPE ($$) != void_type_node)
		    $$ = require_complete_type ($$);
#else
		  $$ = build_method_call ($$, $2, $4, NULL_TREE,
					  LOOKUP_NORMAL);
#endif
		}
	| object unqualified_id LEFT_RIGHT
		{
#if 0
		  /* This is a future direction of this code, but because
		     build_x_function_call cannot always undo what is done
		     in build_component_ref entirely yet, we cannot do this.  */
		  $$ = build_x_function_call (build_component_ref ($$, $2, NULL_TREE, 1), NULL_TREE, current_class_ref);
		  if (TREE_CODE ($$) == CALL_EXPR
		      && TREE_TYPE ($$) != void_type_node)
		    $$ = require_complete_type ($$);
#else
		  $$ = build_method_call ($$, $2, NULL_TREE, NULL_TREE,
					  LOOKUP_NORMAL);
#endif
		}
	| object overqualified_id '(' nonnull_exprlist ')'
		{
		  if (IS_SIGNATURE (OP0 ($2)))
		    {
		      warning ("signature name in scope resolution ignored");
		      $$ = build_method_call ($$, OP1 ($2), $4, NULL_TREE,
					      LOOKUP_NORMAL);
		    }
		  else
		    $$ = build_scoped_method_call ($$, OP0 ($2), OP1 ($2), $4);
		}
	| object overqualified_id LEFT_RIGHT
		{
		  if (IS_SIGNATURE (OP0 ($2)))
		    {
		      warning ("signature name in scope resolution ignored");
		      $$ = build_method_call ($$, OP1 ($2), NULL_TREE, NULL_TREE,
					      LOOKUP_NORMAL);
		    }
		  else
		    $$ = build_scoped_method_call ($$, OP0 ($2), OP1 ($2), NULL_TREE);
		}
	/* p->int::~int() is valid -- 12.4 */
	| object '~' TYPESPEC LEFT_RIGHT
		{
		  if (IDENTIFIER_GLOBAL_VALUE ($3)
		      && (TREE_CODE (TREE_TYPE ($1)) 
			  != TREE_CODE (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE ($3)))))
		    cp_error ("`%E' is not of type `%T'", $1, $3);
		  $$ = cp_convert (void_type_node, $1);
		}
	| object TYPESPEC SCOPE '~' TYPESPEC LEFT_RIGHT
		{
		  if ($2 != $5)
		    cp_error ("destructor specifier `%T::~%T()' must have matching names", $2, $5);
		  if (TREE_CODE (TREE_TYPE ($1))
		      != TREE_CODE (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE ($2))))
		    cp_error ("`%E' is not of type `%T'", $1, $2);
		  $$ = cp_convert (void_type_node, $1);
		}
	| object error
		{
		  $$ = error_mark_node;
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
		{ if (pedantic)
		    pedwarn ("ANSI C++ forbids braced-groups within expressions");
		  $$ = expand_end_stmt_expr ($<ttype>2); }
	| primary_no_id '(' nonnull_exprlist ')'
		{ $$ = build_x_function_call ($$, $3, current_class_ref); }
	| primary_no_id LEFT_RIGHT
		{ $$ = build_x_function_call ($$, NULL_TREE, current_class_ref); }
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

new:
	  NEW
		{ $$ = 0; }
	| global_scope NEW
		{ got_scope = NULL_TREE; $$ = 1; }
	;

delete:
	  DELETE
		{ $$ = 0; }
	| global_scope delete
		{ got_scope = NULL_TREE; $$ = 1; }
	;

boolean.literal:
	  CXX_TRUE
		{ $$ = boolean_true_node; }
	| CXX_FALSE
		{ $$ = boolean_false_node; }
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
		     pair of curly braces of a function.  These are needed
		     for correct operation of dwarfout.c.  */
		  keep_next_level ();
		}
	;

object:
	  primary '.'
		{ got_object = TREE_TYPE ($$); }
	| primary POINTSAT
		{
		  $$ = build_x_arrow ($$); 
		  got_object = TREE_TYPE ($$);
		}
	;

decl:
	  typespec initdecls ';'
		{
		  resume_momentary ($2);
		  if ($1.t && IS_AGGR_TYPE_CODE (TREE_CODE ($1.t)))
		    note_got_semicolon ($1.t);
		}
	| typed_declspecs initdecls ';'
		{
		  resume_momentary ($2);
		  note_list_got_semicolon ($1.t);
		}
	| declmods notype_initdecls ';'
		{ resume_momentary ($2); }
	| typed_declspecs ';'
		{
		  shadow_tag ($1.t);
		  note_list_got_semicolon ($1.t);
		}
	| declmods ';'
		{ warning ("empty declaration"); }
	| extension decl
		{ pedantic = $<itype>1; }
	;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator  %prec EMPTY
	| notype_declarator  %prec EMPTY
	;

/* This is necessary to postpone reduction of `int()()()()'.  */
fcast_or_absdcl:
	  LEFT_RIGHT  %prec EMPTY
		{ $$ = make_call_declarator (NULL_TREE, empty_parms (),
					     NULL_TREE, NULL_TREE); }
	| fcast_or_absdcl LEFT_RIGHT  %prec EMPTY
		{ $$ = make_call_declarator ($$, empty_parms (), NULL_TREE,
					     NULL_TREE); }
	;

/* ANSI type-id (8.1) */
type_id:
	  typed_typespecs absdcl
		{ $$.t = build_decl_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers absdcl
		{ $$.t = build_decl_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec absdcl
		{ $$.t = build_decl_list (get_decl_list ($1.t), $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_typespecs  %prec EMPTY
		{ $$.t = build_decl_list ($1.t, NULL_TREE);
		  $$.new_type_flag = $1.new_type_flag;  }
	| nonempty_cv_qualifiers  %prec EMPTY
		{ $$.t = build_decl_list ($1.t, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

typed_declspecs:
	  typed_typespecs  %prec EMPTY
	| typed_declspecs1
	;

typed_declspecs1:
	  declmods typespec
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t, $1); 
		  $$.new_type_flag = $2.new_type_flag; }
	| typespec reserved_declspecs  %prec HYPERUNARY
		{ $$.t = decl_tree_cons (NULL_TREE, $1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec reserved_typespecquals reserved_declspecs
		{ $$.t = decl_tree_cons (NULL_TREE, $1.t, chainon ($2, $3)); 
		  $$.new_type_flag = $1.new_type_flag; }
	| declmods typespec reserved_declspecs
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t, chainon ($3, $1)); 
		  $$.new_type_flag = $2.new_type_flag; }
	| declmods typespec reserved_typespecquals
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t, chainon ($3, $1)); 
		  $$.new_type_flag = $2.new_type_flag; }
	| declmods typespec reserved_typespecquals reserved_declspecs
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t,
					 chainon ($3, chainon ($4, $1))); 
		  $$.new_type_flag = $2.new_type_flag; }
	;

reserved_declspecs:
	  SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($$));
		  $$ = build_decl_list (NULL_TREE, $$); }
	| reserved_declspecs typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2.t, $$); }
	| reserved_declspecs SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	| reserved_declspecs attributes
		{ $$ = decl_tree_cons ($2, NULL_TREE, $1); }
	| attributes
		{ $$ = decl_tree_cons ($1, NULL_TREE, NULL_TREE); }
	;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

declmods:
	  nonempty_cv_qualifiers  %prec EMPTY
		{ $$ = $1.t; TREE_STATIC ($$) = 1; }
	| SCSPEC
		{ $$ = IDENTIFIER_AS_LIST ($$); }
	| declmods CV_QUALIFIER
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$);
		  TREE_STATIC ($$) = 1; }
	| declmods SCSPEC
		{ if (extra_warnings && TREE_STATIC ($$))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$ = decl_tree_cons (NULL_TREE, $2, $$);
		  TREE_STATIC ($$) = TREE_STATIC ($1); }
	| declmods attributes
		{ $$ = decl_tree_cons ($2, NULL_TREE, $1); }
	| attributes
		{ $$ = decl_tree_cons ($1, NULL_TREE, NULL_TREE); }
	;

/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  typespec  %prec EMPTY
		{ $$.t = get_decl_list ($1.t); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers typespec
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t, $1.t); 
		  $$.new_type_flag = $2.new_type_flag; }
	| typespec reserved_typespecquals
		{ $$.t = decl_tree_cons (NULL_TREE, $1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers typespec reserved_typespecquals
		{ $$.t = decl_tree_cons (NULL_TREE, $2.t, chainon ($3, $1.t)); 
		  $$.new_type_flag = $1.new_type_flag; }
	;

reserved_typespecquals:
	  typespecqual_reserved
		{ $$ = build_decl_list (NULL_TREE, $1.t); }
	| reserved_typespecquals typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2.t, $1); }
	;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec:
	  structsp
	| TYPESPEC  %prec EMPTY
		{ $$.t = $1; $$.new_type_flag = 0; }
	| complete_type_name
		{ $$.t = $1; $$.new_type_flag = 0; }
	| TYPEOF '(' expr ')'
		{ $$.t = TREE_TYPE ($3);
		  $$.new_type_flag = 0; }
	| TYPEOF '(' type_id ')'
		{ $$.t = groktypename ($3.t);
		  $$.new_type_flag = 0; }
	| SIGOF '(' expr ')'
		{ tree type = TREE_TYPE ($3);

                  $$.new_type_flag = 0;
		  if (IS_AGGR_TYPE (type))
		    {
		      sorry ("sigof type specifier");
		      $$.t = type;
		    }
		  else
		    {
		      error ("`sigof' applied to non-aggregate expression");
		      $$.t = error_mark_node;
		    }
		}
	| SIGOF '(' type_id ')'
		{ tree type = groktypename ($3.t);

                  $$.new_type_flag = 0;
		  if (IS_AGGR_TYPE (type))
		    {
		      sorry ("sigof type specifier");
		      $$.t = type;
		    }
		  else
		    {
		      error("`sigof' applied to non-aggregate type");
		      $$.t = error_mark_node;
		    }
		}
	;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved:
	  TYPESPEC
		{ $$.t = $1; $$.new_type_flag = 0; }
	| CV_QUALIFIER
		{ $$.t = $1; $$.new_type_flag = 0; }
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
	  declarator maybeasm maybe_attribute '='
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  if (current_declspecs
		      && TREE_CODE (current_declspecs) != TREE_LIST)
		    current_declspecs = get_decl_list (current_declspecs);
		  if (have_extern_spec && !used_extern_spec)
		    {
		      current_declspecs = decl_tree_cons
			(NULL_TREE, get_identifier ("extern"), 
			 current_declspecs);
		      used_extern_spec = 1;
		    }
		  $<itype>4 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1);
		  cplus_decl_attributes ($<ttype>$, $3, prefix_attributes); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ cp_finish_decl ($<ttype>5, $6, $2, 1, LOOKUP_ONLYCONVERTING);
		  $$ = $<itype>4; }
	| declarator maybeasm maybe_attribute
		{ tree d;
		  split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  if (current_declspecs
		      && TREE_CODE (current_declspecs) != TREE_LIST)
		    current_declspecs = get_decl_list (current_declspecs);
		  if (have_extern_spec && !used_extern_spec)
		    {
		      current_declspecs = decl_tree_cons
			(NULL_TREE, get_identifier ("extern"), 
			 current_declspecs);
		      used_extern_spec = 1;
		    }
		  $$ = suspend_momentary ();
		  d = start_decl ($<ttype>1, current_declspecs, 0);
		  cplus_decl_attributes (d, $3, prefix_attributes);
		  cp_finish_decl (d, NULL_TREE, $2, 1, 0); }
	;

initdcl:
	  declarator maybeasm maybe_attribute '='
		{ $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1);
		  cplus_decl_attributes ($<ttype>$, $3, prefix_attributes); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ cp_finish_decl ($<ttype>5, $6, $2, 1, LOOKUP_ONLYCONVERTING); }
	| declarator maybeasm maybe_attribute
		{ $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 0);
		  cplus_decl_attributes ($<ttype>$, $3, prefix_attributes);
		  cp_finish_decl ($<ttype>$, NULL_TREE, $2, 1, 0); }
	;

notype_initdcl0:
	  notype_declarator maybeasm maybe_attribute '='
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<itype>4 = suspend_momentary ();
		  $<ttype>$ = start_decl ($<ttype>1, current_declspecs, 1);
		  cplus_decl_attributes ($<ttype>$, $3, prefix_attributes); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ cp_finish_decl ($<ttype>5, $6, $2, 1, LOOKUP_ONLYCONVERTING);
		  $$ = $<itype>4; }
	| notype_declarator maybeasm maybe_attribute
		{ tree d;
		  split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $$ = suspend_momentary ();
		  d = start_decl ($<ttype>1, current_declspecs, 0);
		  cplus_decl_attributes (d, $3, prefix_attributes);
		  cp_finish_decl (d, NULL_TREE, $2, 1, 0); }
	;

nomods_initdcl0:
	  notype_declarator maybeasm maybe_attribute '='
		{ current_declspecs = NULL_TREE;
		  prefix_attributes = NULL_TREE;
		  $<itype>4 = suspend_momentary ();
		  $<ttype>$ = start_decl ($1, current_declspecs, 1);
		  cplus_decl_attributes ($<ttype>$, $3, prefix_attributes); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ cp_finish_decl ($<ttype>5, $6, $2, 1, LOOKUP_ONLYCONVERTING);
		  $$ = $<itype>4; }
	| notype_declarator maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = NULL_TREE;
		  prefix_attributes = NULL_TREE;
		  $$ = suspend_momentary ();
		  d = start_decl ($1, current_declspecs, 0);
		  cplus_decl_attributes (d, $3, prefix_attributes);
		  cp_finish_decl (d, NULL_TREE, $2, 1, 0); }
	;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile.  */
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
		{ $$ = $1; }
	| attribute_list ',' attrib
		{ $$ = chainon ($1, $3); }
	;
 
attrib:
	  /* empty */
		{ $$ = NULL_TREE; }
	| any_word
		{ $$ = build_tree_list ($1, NULL_TREE); }
	| any_word '(' IDENTIFIER ')'
		{ $$ = build_tree_list ($1, build_tree_list (NULL_TREE, $3)); }
	| any_word '(' IDENTIFIER ',' nonnull_exprlist ')'
		{ $$ = build_tree_list ($1, tree_cons (NULL_TREE, $3, $5)); }
	| any_word '(' nonnull_exprlist ')'
		{ $$ = build_tree_list ($1, $3); }
	;

/* This still leaves out most reserved keywords,
   shouldn't we include them?  */

any_word:
	  identifier
	| SCSPEC
	| TYPESPEC
	| CV_QUALIFIER
	;

/* A nonempty list of identifiers, including typenames.  */
identifiers_or_typenames:
	  identifier
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| identifiers_or_typenames ',' identifier
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;

maybe_init:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| '=' init
		{ $$ = $2; }

/* If we are processing a template, we don't want to expand this
   initializer yet.  */

init:
	  expr_no_commas  %prec '='
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
		{ $$ = expr_tree_cons (NULL_TREE, $3, $$); }
	/* These are for labeled elements.  */
	| '[' expr_no_commas ']' init
		{ $$ = build_expr_list ($2, $4); }
	| initlist ',' CASE expr_no_commas ':' init
		{ $$ = expr_tree_cons ($4, $6, $$); }
	| identifier ':' init
		{ $$ = build_expr_list ($$, $3); }
	| initlist ',' identifier ':' init
		{ $$ = expr_tree_cons ($3, $5, $$); }
	;

fn.defpen:
	PRE_PARSED_FUNCTION_DECL
		{ start_function (NULL_TREE, TREE_VALUE ($1),
				  NULL_TREE, 1);
		  reinit_parse_for_function (); }

pending_inline:
	  fn.defpen maybe_return_init ctor_initializer_opt compstmt_or_error
		{
		  int nested = (hack_decl_function_context
				(current_function_decl) != NULL_TREE);
		  finish_function (lineno, (int)$3, nested);
		  process_next_inline ($1);
		}
	| fn.defpen maybe_return_init function_try_block
		{ process_next_inline ($1); }
	| fn.defpen maybe_return_init error
		{ process_next_inline ($1); }
	;

pending_inlines:
	/* empty */
	| pending_inlines pending_inline eat_saved_input
	;

/* A regurgitated default argument.  The value of DEFARG_MARKER will be
   the TREE_LIST node for the parameter in question.  */
defarg_again:
	DEFARG_MARKER expr_no_commas END_OF_SAVED_INPUT
		{ replace_defarg ($1, $2); }
	| DEFARG_MARKER error END_OF_SAVED_INPUT
		{ replace_defarg ($1, error_mark_node); }

pending_defargs:
	  /* empty */ %prec EMPTY
	| pending_defargs defarg_again
		{ do_pending_defargs (); }
	| pending_defargs error
		{ do_pending_defargs (); }
	;

structsp:
	  ENUM identifier '{'
		{ $<itype>3 = suspend_momentary ();
		  $<ttype>$ = start_enum ($2); }
	  enumlist maybecomma_warn '}'
		{ $$.t = finish_enum ($<ttype>4, $5);
		  $$.new_type_flag = 1;
		  resume_momentary ((int) $<itype>3);
		  check_for_missing_semicolon ($<ttype>4); }
	| ENUM identifier '{' '}'
		{ $$.t = finish_enum (start_enum ($2), NULL_TREE);
		  $$.new_type_flag = 1;
		  check_for_missing_semicolon ($$.t); }
	| ENUM '{'
		{ $<itype>2 = suspend_momentary ();
		  $<ttype>$ = start_enum (make_anon_name ()); }
	  enumlist maybecomma_warn '}'
		{ $$.t = finish_enum ($<ttype>3, $4);
		  resume_momentary ((int) $<itype>1);
		  check_for_missing_semicolon ($<ttype>3);
		  $$.new_type_flag = 1; }
	| ENUM '{' '}'
		{ $$.t = finish_enum (start_enum (make_anon_name()), NULL_TREE);
		  $$.new_type_flag = 1;
		  check_for_missing_semicolon ($$.t); }
	| ENUM identifier
		{ $$.t = xref_tag (enum_type_node, $2, NULL_TREE, 1); 
		  $$.new_type_flag = 0; }
	| ENUM complex_type_name
		{ $$.t = xref_tag (enum_type_node, $2, NULL_TREE, 1); 
		  $$.new_type_flag = 0; }
	| TYPENAME_KEYWORD typename_sub
		{ $$.t = $2;
		  $$.new_type_flag = 0; }
	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head left_curly 
                { reset_specialization(); }
          opt.component_decl_list '}' maybe_attribute
		{
		  int semi;
		  tree id;

		  $<ttype>$ = $1;
#if 0
		  /* Need to rework class nesting in the
		     presence of nested classes, etc.  */
		  shadow_tag (CLASSTYPE_AS_LIST ($1)); */
#endif
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  semi = yychar == ';';
		  /* finish_struct nukes this anyway; if
		     finish_exception does too, then it can go.  */
		  if (semi)
		    note_got_semicolon ($1);

		  if (TREE_CODE ($1) == ENUMERAL_TYPE)
		    ;
		  else
		    {
		      $<ttype>$ = finish_struct ($1, $4, $6, semi);
		      if (semi) note_got_semicolon ($<ttype>$);
		    }

		  pop_obstacks ();

		  if (! semi)
		    check_for_missing_semicolon ($1); 
		  if (current_scope () == current_function_decl)
		    do_pending_defargs ();
		}
	  pending_defargs
		{
		  if (pending_inlines 
		      && current_scope () == current_function_decl)
		    do_pending_inlines ();
		}
	  pending_inlines
		{ 
		  $$.t = $<ttype>7;
		  $$.new_type_flag = 1; 
		  if (current_class_type == NULL_TREE)
		    clear_inline_text_obstack (); 
		}
	| class_head  %prec EMPTY
		{
		  $$.t = $1;
		  $$.new_type_flag = 0;
		  /* struct B: public A; is not accepted by the WP grammar.  */
		  if (TYPE_BINFO_BASETYPES ($$.t) && !TYPE_SIZE ($$.t)
		      && ! TYPE_BEING_DEFINED ($$.t))
		    cp_error ("base clause without member specification for `%#T'",
			      $$.t);
		}
	;

maybecomma:
	  /* empty */
	| ','
	;

maybecomma_warn:
	  /* empty */
	| ','
		{ if (pedantic && !in_system_header)
		    pedwarn ("comma at end of enumerator list"); }
	;

aggr:
	  AGGR
	| aggr SCSPEC
		{ error ("storage class specifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr TYPESPEC
		{ error ("type specifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr CV_QUALIFIER
		{ error ("type qualifier `%s' not allowed after struct or class", IDENTIFIER_POINTER ($2)); }
	| aggr AGGR
		{ error ("no body nor ';' separates two class, struct or union declarations"); }
	;

named_class_head_sans_basetype:
	  aggr identifier
		{ current_aggr = $$; $$ = $2; }
	;

named_class_head_sans_basetype_defn:
	  aggr identifier_defn  %prec EMPTY
		{ current_aggr = $$; $$ = $2; }
	;

named_complex_class_head_sans_basetype:
	  aggr nested_name_specifier identifier
		{
		  current_aggr = $1;
		  $$ = handle_class_head ($1, $2, $3);
		}
	| aggr global_scope nested_name_specifier identifier
		{
		  current_aggr = $1;
		  $$ = handle_class_head ($1, $3, $4);
		}
	| aggr global_scope identifier
		{
		  current_aggr = $1;
		  $$ = handle_class_head ($1, NULL_TREE, $3);
		}
	| aggr template_type
		{ current_aggr = $$; $$ = $2; }
	| aggr nested_name_specifier template_type
		{ current_aggr = $$; $$ = $3; }
	;

do_xref_defn:
	  /* empty */  %prec EMPTY
		{ $<ttype>$ = xref_tag (current_aggr, $<ttype>0, NULL_TREE, 0); }
	;

named_class_head:
	  named_class_head_sans_basetype  %prec EMPTY
		{ $$ = xref_tag (current_aggr, $1, NULL_TREE, 1); }
	| named_class_head_sans_basetype_defn do_xref_defn
          maybe_base_class_list  %prec EMPTY
		{ 
		  $$ = $<ttype>2;
		  if ($3)
                    xref_basetypes (current_aggr, $1, $<ttype>2, $3); 
		}
	| named_complex_class_head_sans_basetype maybe_base_class_list
		{ 
		  $$ = TREE_TYPE ($1);
		  if (TREE_INT_CST_LOW (current_aggr) == union_type 
		      && TREE_CODE ($$) != UNION_TYPE)
		    cp_pedwarn ("`union' tag used in declaring `%#T'", $$);
		  else if (TREE_CODE ($$) == UNION_TYPE
			   && TREE_INT_CST_LOW (current_aggr) != union_type)
		    cp_pedwarn ("non-`union' tag used in declaring `%#T'", $$);
		  if ($2)
		    {
		      if (IS_AGGR_TYPE ($$) && CLASSTYPE_USE_TEMPLATE ($$))
		        {
		          if (CLASSTYPE_IMPLICIT_INSTANTIATION ($$)
			      && TYPE_SIZE ($$) == NULL_TREE)
			    {
			      SET_CLASSTYPE_TEMPLATE_SPECIALIZATION ($$);
			      if (processing_template_decl)
				push_template_decl (TYPE_MAIN_DECL ($$));
			    }
			  else if (CLASSTYPE_TEMPLATE_INSTANTIATION ($$))
			    cp_error ("specialization after instantiation of `%T'", $$);
			}
		      xref_basetypes (current_aggr, $1, $$, $2); 
		    }
		}
	;

unnamed_class_head:
	  aggr '{'
		{ $$ = xref_tag ($$, make_anon_name (), NULL_TREE, 0);
		  yyungetc ('{', 1); }
	;

class_head:
	  unnamed_class_head
	| named_class_head
	;

maybe_base_class_list:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| ':' see_typename  %prec EMPTY
		{ yyungetc(':', 1); $$ = NULL_TREE; }
	| ':' see_typename base_class_list  %prec EMPTY
		{ $$ = $3; }
	;

base_class_list:
	  base_class
	| base_class_list ',' see_typename base_class
		{ $$ = chainon ($$, $4); }
	;

base_class:
	  base_class.1
		{
		  tree type = TREE_TYPE ($1);
		  if (! is_aggr_type (type, 1))
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
		      $$ = build_tree_list (access_public_node, type);
		    }
		  else if (type && IS_SIGNATURE (type))
		    {
		      error ("signature name not allowed as base class");
		      $$ = NULL_TREE;
		    }
		  else
		    $$ = build_tree_list (access_default_node, type);
		}
	| base_class_access_list see_typename base_class.1
		{
		  tree type = TREE_TYPE ($3);
		  if (current_aggr == signature_type_node)
		    error ("access and source specifiers not allowed in signature");
		  if (! IS_AGGR_TYPE (type))
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
		      $$ = build_tree_list (access_public_node, type);
		    }
		  else if (type && IS_SIGNATURE (type))
		    {
		      error ("signature name not allowed as base class");
		      $$ = NULL_TREE;
		    }
		  else
		    $$ = build_tree_list ($$, type);
		}
	;

base_class.1:
	  typename_sub
		{ $$ = TYPE_MAIN_DECL ($1); }
	| nonnested_type
	| SIGOF '(' expr ')'
		{
		  if (current_aggr == signature_type_node)
		    {
		      if (IS_AGGR_TYPE (TREE_TYPE ($3)))
			{
			  sorry ("`sigof' as base signature specifier");
			  $$ = TREE_TYPE ($3);
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
		      if (IS_AGGR_TYPE (groktypename ($3.t)))
			{
			  sorry ("`sigof' as base signature specifier");
			  $$ = groktypename ($3.t);
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
	  VISSPEC see_typename
	| SCSPEC see_typename
		{ if ($<ttype>$ != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual access");
		  $$ = access_default_virtual_node; }
	| base_class_access_list VISSPEC see_typename
		{ int err = 0;
		  if ($2 == access_protected_node)
		    {
		      warning ("`protected' access not implemented");
		      $2 = access_public_node;
		      err++;
		    }
		  else if ($2 == access_public_node)
		    {
		      if ($1 == access_private_node)
			{
			mixed:
			  error ("base class cannot be public and private");
			}
		      else if ($1 == access_default_virtual_node)
			$$ = access_public_virtual_node;
		    }
		  else /* $2 == access_private_node */
		    {
		      if ($1 == access_public_node)
			goto mixed;
		      else if ($1 == access_default_virtual_node)
			$$ = access_private_virtual_node;
		    }
		}
	| base_class_access_list SCSPEC see_typename
		{ if ($2 != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual access");
		  if ($$ == access_public_node)
		    $$ = access_public_virtual_node;
		  else if ($$ == access_private_node)
		    $$ = access_private_virtual_node; }
	;

left_curly:
	  '{'
		{ tree t = $<ttype>0;
		  push_obstacks_nochange ();
		  end_temporary_allocation ();

		  if (t == error_mark_node
		      || ! IS_AGGR_TYPE (t))
		    {
		      t = $<ttype>0 = make_lang_type (RECORD_TYPE);
		      pushtag (make_anon_name (), t, 0);
		    }
		  if (TYPE_SIZE (t))
		    duplicate_tag_error (t);
                  if (TYPE_SIZE (t) || TYPE_BEING_DEFINED (t))
                    {
                      t = make_lang_type (TREE_CODE (t));
                      pushtag (TYPE_IDENTIFIER ($<ttype>0), t, 0);
                      $<ttype>0 = t;
                    }
		  if (processing_template_decl && TYPE_CONTEXT (t)
		      && ! current_class_type)
		    push_template_decl (TYPE_STUB_DECL (t));
		  pushclass (t, 0);
		  TYPE_BEING_DEFINED (t) = 1;
		  if (IS_AGGR_TYPE (t) && CLASSTYPE_USE_TEMPLATE (t))
		    {
		      if (CLASSTYPE_IMPLICIT_INSTANTIATION (t)
			  && TYPE_SIZE (t) == NULL_TREE)
			{
			  SET_CLASSTYPE_TEMPLATE_SPECIALIZATION (t);
			  if (processing_template_decl)
			    push_template_decl (TYPE_MAIN_DECL (t));
			}
		      else if (CLASSTYPE_TEMPLATE_INSTANTIATION (t))
			cp_error ("specialization after instantiation of `%T'", t);
		    }
		  /* Reset the interface data, at the earliest possible
		     moment, as it might have been set via a class foo;
		     before.  */
		  /* Don't change signatures.  */
		  if (! IS_SIGNATURE (t))
		    {
		      extern tree pending_vtables;
		      int needs_writing;
		      tree name = TYPE_IDENTIFIER (t);

		      if (! ANON_AGGRNAME_P (name))
			{
			  CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
			  SET_CLASSTYPE_INTERFACE_UNKNOWN_X
			    (t, interface_unknown);
			}

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

self_reference:
	  /* empty */
		{
		    $$ = build_self_reference ();
		}
	;

opt.component_decl_list:
	  self_reference
		{ if ($$) $$ = build_tree_list (access_public_node, $$); }
	| self_reference component_decl_list
		{
		  if (current_aggr == signature_type_node)
		    $$ = build_tree_list (access_public_node, $2);
		  else
		    $$ = build_tree_list (access_default_node, $2);
		  if ($1) $$ = tree_cons (access_public_node, $1, $$);
		}
	| opt.component_decl_list VISSPEC ':' component_decl_list
		{
		  tree visspec = $2;

		  if (current_aggr == signature_type_node)
		    {
		      error ("access specifier not allowed in signature");
		      visspec = access_public_node;
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
	;

component_decl:
	  component_decl_1 ';'
		{ }
	| component_decl_1 '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0); }
	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 TRY /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 RETURN /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 '{' /* nodecls compstmt */
		{ $$ = finish_method ($$); }
	| ';'
		{ $$ = NULL_TREE; }
	| extension component_decl
		{ $$ = $2;
		  pedantic = $<itype>1; }
	;

component_decl_1:
	/* Do not add a "typed_declspecs declarator" rule here for
	   speed; we need to call grok_x_components for enums, so the
	   speedup would be insignificant.  */
	  typed_declspecs components
		{ $$ = grok_x_components ($1.t, $2); }
	| declmods notype_components
		{ $$ = grok_x_components ($1, $2); }
	| notype_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2,
				  build_tree_list ($3, NULL_TREE)); }
	| constructor_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2,
				  build_tree_list ($3, NULL_TREE)); }
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
	| declmods component_constructor_declarator maybeasm maybe_attribute maybe_init
		{ tree specs, attrs;
		  split_specs_attrs ($1, &specs, &attrs);
		  $$ = grokfield ($2, specs, $5, $3,
				  build_tree_list ($4, attrs)); }
	| component_constructor_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2,
				  build_tree_list ($3, NULL_TREE)); }
	| using_decl
		{ $$ = do_class_using_decl ($1); }
        | template_header component_decl_1 
                { 
                  if ($1)
		    end_template_decl (); 
                  else
                    end_specialization ();

		  if ($2 && DECL_TEMPLATE_INFO ($2)
		      && !DECL_TEMPLATE_SPECIALIZATION ($2))
		    {
		      $$ = DECL_TI_TEMPLATE ($2); 
		      check_member_template ($$);
		    }
		  else if ($2)
		    $$ = $2;
		  else
		    {
		      cp_error("invalid member template declaration");
		      $$ = NULL_TREE;
		    }
		}

/* The case of exactly one component is handled directly by component_decl.  */
/* ??? Huh? ^^^ */
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
	  after_type_declarator maybeasm maybe_attribute maybe_init
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokfield ($$, current_declspecs, $4, $2,
				  build_tree_list ($3, prefix_attributes)); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4, prefix_attributes); }
	;

notype_component_declarator0:
	  notype_declarator maybeasm maybe_attribute maybe_init
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokfield ($$, current_declspecs, $4, $2,
				  build_tree_list ($3, prefix_attributes)); }
	| constructor_declarator maybeasm maybe_attribute maybe_init
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokfield ($$, current_declspecs, $4, $2,
				  build_tree_list ($3, prefix_attributes)); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4, prefix_attributes); }
	| ':' expr_no_commas maybe_attribute
		{ split_specs_attrs ($<ttype>0, &current_declspecs,
				     &prefix_attributes);
		  $<ttype>0 = current_declspecs;
		  $$ = grokbitfield (NULL_TREE, current_declspecs, $2);
		  cplus_decl_attributes ($$, $3, prefix_attributes); }
	;

after_type_component_declarator:
	  after_type_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, current_declspecs, $4, $2,
				  build_tree_list ($3, prefix_attributes)); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4, prefix_attributes); }
	;

notype_component_declarator:
	  notype_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, current_declspecs, $4, $2,
				  build_tree_list ($3, prefix_attributes)); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield ($$, current_declspecs, $3);
		  cplus_decl_attributes ($$, $4, prefix_attributes); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = grokbitfield (NULL_TREE, current_declspecs, $2);
		  cplus_decl_attributes ($$, $3, prefix_attributes); }
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
		{ $$.t = build_decl_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| type_specifier_seq  %prec EMPTY
		{ $$.t = build_decl_list ($1.t, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	/* GNU extension to allow arrays of arbitrary types with
	   non-constant dimension.  */
	| '(' type_id ')' '[' expr ']'
		{
		  if (pedantic)
		    pedwarn ("ANSI C++ forbids array dimensions with parenthesized type in new");
		  $$.t = build_parse_node (ARRAY_REF, TREE_VALUE ($2.t), $5);
		  $$.t = build_decl_list (TREE_PURPOSE ($2.t), $$.t);
		  $$.new_type_flag = $2.new_type_flag;
		}
	;

cv_qualifiers:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| cv_qualifiers CV_QUALIFIER
		{ $$ = decl_tree_cons (NULL_TREE, $2, $$); }
	;

nonempty_cv_qualifiers:
	  CV_QUALIFIER
		{ $$.t = IDENTIFIER_AS_LIST ($1); 
		  $$.new_type_flag = 0; }
	| nonempty_cv_qualifiers CV_QUALIFIER
		{ $$.t = decl_tree_cons (NULL_TREE, $2, $1.t); 
		  $$.new_type_flag = $1.new_type_flag; }
	;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are preferred.  */

suspend_mom:
	  /* empty */
		{ $<itype>$ = suspend_momentary (); } 

/* An expression which will not live on the momentary obstack.  */
nonmomentary_expr:
	  suspend_mom expr
		{ resume_momentary ((int) $<itype>1); $$ = $2; }
	;

/* An expression which will not live on the momentary obstack.  */
maybe_parmlist:
	  suspend_mom '(' nonnull_exprlist ')'
		{ resume_momentary ((int) $<itype>1); $$ = $3; }
	| suspend_mom '(' parmlist ')'
		{ resume_momentary ((int) $<itype>1); $$ = $3; }
	| suspend_mom LEFT_RIGHT
		{ resume_momentary ((int) $<itype>1); $$ = empty_parms (); }
	| suspend_mom '(' error ')'
		{ resume_momentary ((int) $<itype>1); $$ = NULL_TREE; }
	;

/* A declarator that is allowed only after an explicit typespec.  */
/* may all be followed by prec '.' */
after_type_declarator:
	  '*' nonempty_cv_qualifiers after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers after_type_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_after_type_declarator
	;

nonnested_type:
	  type_name  %prec EMPTY
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    {
		      if (current_class_type
			  && TYPE_BEING_DEFINED (current_class_type)
			  && ! IDENTIFIER_CLASS_VALUE ($1))
			{
			  /* Be sure to get an inherited typedef.  */
			  $$ = lookup_name ($1, 1);
			  /* Remember that this name has been used in the class
			     definition, as per [class.scope0] */
			  pushdecl_class_level ($$);
			}
		      else
			$$ = identifier_typedecl_value ($1);
		    }
		  else
		    $$ = $1;
		}
	| global_scope type_name
		{
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    $$ = identifier_typedecl_value ($2);
		  else
		    $$ = $2;
		  got_scope = NULL_TREE;
		}
	;

complete_type_name:
	  nonnested_type
	| nested_type
	| global_scope nested_type
		{ $$ = $2; }
	;

nested_type:
	  nested_name_specifier type_name  %prec EMPTY
		{ $$ = get_type_decl ($2); }
	;

direct_after_type_declarator:
	  direct_after_type_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $2, $3, $4); }
	| direct_after_type_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_after_type_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	| '(' after_type_declarator ')'
		{ $$ = $2; }
	| nested_name_specifier type_name  %prec EMPTY
		{ push_nested_class ($1, 3);
		  $$ = build_parse_node (SCOPE_REF, $$, $2);
		  TREE_COMPLEXITY ($$) = current_class_depth; }
	| type_name  %prec EMPTY
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers notype_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_notype_declarator
	;

complex_notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' complex_notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' complex_notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers notype_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| complex_direct_notype_declarator
	;

complex_direct_notype_declarator:
	  direct_notype_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $2, $3, $4); }
	| '(' complex_notype_declarator ')'
		{ $$ = $2; }
	| direct_notype_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_notype_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	| notype_qualified_id
		{ if (OP0 ($$) != current_class_type)
		    {
		      push_nested_class (OP0 ($$), 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
        | nested_name_specifier notype_template_declarator
                { got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $1, $2);
		  if ($1 != current_class_type)
		    {
		      push_nested_class ($1, 3);
		      TREE_COMPLEXITY ($$) = current_class_depth;
		    }
		}
	;

qualified_id:
	  nested_name_specifier unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $$, $2); }
        | nested_name_specifier object_template_id
                { got_scope = NULL_TREE;
 		  $$ = build_parse_node (SCOPE_REF, $1, $2); }
	;

notype_qualified_id:
	  nested_name_specifier notype_unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $$, $2); }
        | nested_name_specifier object_template_id
                { got_scope = NULL_TREE;
		  $$ = build_parse_node (SCOPE_REF, $1, $2); }
	;

overqualified_id:
	  notype_qualified_id
	| global_scope notype_qualified_id
		{ $$ = $2; }
	;

functional_cast:
	  typespec '(' nonnull_exprlist ')'
		{ $$ = build_functional_cast ($1.t, $3); }
	| typespec '(' expr_or_declarator ')'
		{ $$ = reparse_decl_as_expr ($1.t, $3); }
	| typespec fcast_or_absdcl  %prec EMPTY
		{ $$ = reparse_absdcl_as_expr ($1.t, $2); }
	;

type_name:
	  TYPENAME
	| SELFNAME
	| template_type  %prec EMPTY
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
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    {
		      $$ = lastiddecl;
		      /* Remember that this name has been used in the class
			 definition, as per [class.scope0] */
		      if (current_class_type
			  && TYPE_BEING_DEFINED (current_class_type)
			  && ! IDENTIFIER_CLASS_VALUE ($1))
			pushdecl_class_level ($$);
		    }
		  got_scope = $$ = TREE_TYPE ($$);
		}
	| SELFNAME SCOPE
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$ = TREE_TYPE ($$);
		}
	| NSNAME SCOPE
		{
		  if (TREE_CODE ($$) == IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$;
		}
	| template_type SCOPE
		{ got_scope = $$ = complete_type (TREE_TYPE ($1)); }
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

typename_sub:
	  typename_sub0
	| global_scope typename_sub0
		{ $$ = $2; }
	;

typename_sub0:
	  typename_sub1 identifier
		{
		  if (TREE_CODE_CLASS (TREE_CODE ($1)) == 't')
		    $$ = make_typename_type ($1, $2);
		  else if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    cp_error ("`%T' is not a class or namespace", $2);
		  else
		    $$ = $2;
		}
	;

typename_sub1:
	  typename_sub2
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    cp_error ("`%T' is not a class or namespace", $1);
		}
	| typename_sub1 typename_sub2
		{
		  if (TREE_CODE_CLASS (TREE_CODE ($1)) == 't')
		    $$ = make_typename_type ($1, $2);
		  else if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    cp_error ("`%T' is not a class or namespace", $2);
		  else
		    $$ = $2;
		}
	;

typename_sub2:
	  TYPENAME SCOPE
		{
		  if (TREE_CODE ($1) != IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$ = complete_type (TREE_TYPE ($$));
		}
	| SELFNAME SCOPE
		{
		  if (TREE_CODE ($1) != IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$ = complete_type (TREE_TYPE ($$));
		}
	| template_type SCOPE
		{ got_scope = $$ = complete_type (TREE_TYPE ($$)); }
	| PTYPENAME SCOPE
	| IDENTIFIER SCOPE
	| NSNAME SCOPE
		{
		  if (TREE_CODE ($$) == IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$;
		}
	;

complex_type_name:
	  global_scope type_name
		{
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    $$ = identifier_typedecl_value ($2);
		  else
		    $$ = $2;
		  got_scope = NULL_TREE;
		}
	| nested_type
	| global_scope nested_type
		{ $$ = $2; }
	;

ptr_to_mem:
	  nested_name_specifier '*'
		{ got_scope = NULL_TREE; }
	| global_scope nested_name_specifier '*'
		{ $$ = $2; got_scope = NULL_TREE; }
	;

/* All uses of explicit global scope must go through this nonterminal so
   that got_scope will be set before yylex is called to get the next token.  */
global_scope:
	  SCOPE
		{ got_scope = void_type_node; }
	;

/* ANSI new-declarator (5.3.4) */
new_declarator:
	  '*' cv_qualifiers new_declarator
		{ $$ = make_pointer_declarator ($2, $3); }
	| '*' cv_qualifiers  %prec EMPTY
		{ $$ = make_pointer_declarator ($2, NULL_TREE); }
	| '&' cv_qualifiers new_declarator  %prec EMPTY
		{ $$ = make_reference_declarator ($2, $3); }
	| '&' cv_qualifiers  %prec EMPTY
		{ $$ = make_reference_declarator ($2, NULL_TREE); }
	| ptr_to_mem cv_qualifiers  %prec EMPTY
		{ tree arg = make_pointer_declarator ($2, NULL_TREE);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem cv_qualifiers new_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_new_declarator  %prec EMPTY
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
	  '*' nonempty_cv_qualifiers absdcl
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '*' absdcl
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '*' nonempty_cv_qualifiers  %prec EMPTY
		{ $$ = make_pointer_declarator ($2.t, NULL_TREE); }
	| '*'  %prec EMPTY
		{ $$ = make_pointer_declarator (NULL_TREE, NULL_TREE); }
	| '&' nonempty_cv_qualifiers absdcl
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '&' absdcl
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| '&' nonempty_cv_qualifiers  %prec EMPTY
		{ $$ = make_reference_declarator ($2.t, NULL_TREE); }
	| '&'  %prec EMPTY
		{ $$ = make_reference_declarator (NULL_TREE, NULL_TREE); }
	| ptr_to_mem cv_qualifiers  %prec EMPTY
		{ tree arg = make_pointer_declarator ($2, NULL_TREE);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem cv_qualifiers absdcl
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	| direct_abstract_declarator  %prec EMPTY
	;

/* ANSI direct-abstract-declarator (8.1) */
direct_abstract_declarator:
	  '(' absdcl ')'
		{ $$ = $2; }
	  /* `(typedef)1' is `int'.  */
	| PAREN_STAR_PAREN
	| direct_abstract_declarator '(' parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $3, $5, $6); }
	| direct_abstract_declarator LEFT_RIGHT cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, empty_parms (), $3, $4); }
	| direct_abstract_declarator '[' nonmomentary_expr ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $$, $3); }
	| direct_abstract_declarator '[' ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $$, NULL_TREE); }
	| '(' complex_parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator (NULL_TREE, $2, $4, $5); }
	| regcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
		{ set_quals_and_spec ($$, $2, $3); }
	| fcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
		{ set_quals_and_spec ($$, $2, $3); }
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

errstmt:
	  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

.pushlevel:
	  /* empty */
		{ do_pushlevel (); }
	;

.poplevel:
	  /* empty */
		{ $$ = do_poplevel (); }
	;

/* Read zero or more forward-declarations for labels
   that nested functions can jump to.  */
maybe_label_decls:
	  /* empty */
	| label_decls
		{ if (pedantic)
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

compstmt:
	  '{'
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (COMPOUND_STMT, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		}
	  .pushlevel compstmtend .poplevel
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  $$ = $5;
		}
	;

simple_if:
	  IF
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (IF_STMT, NULL_TREE, NULL_TREE,
					        NULL_TREE);
		      add_tree ($<ttype>$);
		    }
                  cond_stmt_keyword = "if";
		}
	  .pushlevel paren_cond_or_null
		{
		  if (processing_template_decl)
		    {
		      if (last_tree != $<ttype>2)
		        {
			  TREE_OPERAND ($<ttype>2, 0) = last_tree;
			  TREE_CHAIN ($<ttype>2) = NULL_TREE;
			  last_tree = $<ttype>2;
			}
		      else
		        TREE_OPERAND ($<ttype>2, 0) = $4;
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      expand_start_cond ($4, 0);
		    }
		}
	  implicitly_scoped_stmt
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 1) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      $<ttype>$ = last_tree = $<ttype>2;
		    }
		}
	;

implicitly_scoped_stmt:
	  compstmt
		{ finish_stmt (); }
	| .pushlevel
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (COMPOUND_STMT, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		}
	  simple_stmt .poplevel
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  $$ = $4;
		}
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
		  if (! processing_template_decl)
		    {
		      emit_line_note (input_filename, lineno);
		      /* Do default conversion if safe and possibly important,
		         in case within ({...}).  */
		      if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
		           && lvalue_p (expr))
		          || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
		        expr = default_conversion (expr);
		    }
		  cplus_expand_expr_stmt (expr);
		  clear_momentary ();
		  finish_stmt (); }
	| simple_if ELSE
		{ if (! processing_template_decl) expand_start_else (); }
	  implicitly_scoped_stmt
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>1, 2) = TREE_CHAIN ($<ttype>1);
		      TREE_CHAIN ($<ttype>1) = NULL_TREE;
		      last_tree = $<ttype>1;
		    }
		  else
		    expand_end_cond ();
		}
	  .poplevel
		{ finish_stmt (); }
	| simple_if  %prec IF
		{ if (! processing_template_decl) expand_end_cond ();
		  do_poplevel ();
		  finish_stmt (); }
	| WHILE
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (WHILE_STMT, NULL_TREE, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		  else
		    {
		      emit_nop ();
		      emit_line_note (input_filename, lineno);
		      expand_start_loop (1); 
		    }
		  cond_stmt_keyword = "while";
		}
	  .pushlevel paren_cond_or_null
		{
		  if (processing_template_decl)
		    {
		      if (last_tree != $<ttype>2)
		        {
			  TREE_OPERAND ($<ttype>2, 0) = last_tree;
			  TREE_CHAIN ($<ttype>2) = NULL_TREE;
			  last_tree = $<ttype>2;
			}
		      else
		        TREE_OPERAND ($<ttype>2, 0) = $4;
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      expand_exit_loop_if_false (0, $4);
		    }
		}
	  already_scoped_stmt .poplevel
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 1) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    expand_end_loop ();
		  finish_stmt ();
		}
	| DO
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (DO_STMT, NULL_TREE, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		  else
		    {
		      emit_nop ();
		      emit_line_note (input_filename, lineno);
		      expand_start_loop_continue_elsewhere (1);
		    }
		}
	  implicitly_scoped_stmt WHILE
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    {
		      expand_loop_continue_here ();
		      cond_stmt_keyword = "do";
		    }
		}
	  paren_expr_or_null ';'
		{
		  if (processing_template_decl)
		    TREE_OPERAND ($<ttype>2, 1) = $6;
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      expand_exit_loop_if_false (0, $6);
		      expand_end_loop ();
		    }
		  clear_momentary ();
		  finish_stmt ();
		}
	| FOR
		{ if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (FOR_STMT, NULL_TREE, NULL_TREE, 
					        NULL_TREE, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
                  else
		    emit_line_note (input_filename, lineno);
		  if (flag_new_for_scope > 0)
		    {
		      /* Conditionalize .pushlevel */
		      pushlevel (0);
		      note_level_for_for ();
		      clear_last_expr ();
		      push_momentary ();
		      expand_start_bindings (0);
		    }
		}
	  '(' for.init.statement
		{
		  if (processing_template_decl)
		    {
		      if (last_tree != $<ttype>2)
			{
			  TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
			  TREE_CHAIN ($<ttype>2) = NULL_TREE;
			  last_tree = $<ttype>2;
			}
		    }
		  else
		    {
		      emit_nop ();
		      emit_line_note (input_filename, lineno);
		      expand_start_loop_continue_elsewhere (1); 
		    }
		}
	  .pushlevel xcond ';'
		{
		  if (processing_template_decl)
		    {
		      if (last_tree != $<ttype>2)
		        {
			  TREE_OPERAND ($<ttype>2, 1) = last_tree;
			  TREE_CHAIN ($<ttype>2) = NULL_TREE;
			  last_tree = $<ttype>2;
			}
		      else
		        TREE_OPERAND ($<ttype>2, 1) = $7;
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      if ($7) expand_exit_loop_if_false (0, $7);
		    }
		}
	  xexpr ')'
		/* Don't let the tree nodes for $10 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
		{
		  if (processing_template_decl)
		    TREE_OPERAND ($<ttype>2, 2) = $10;
		  push_momentary ();
		}
	  already_scoped_stmt .poplevel
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 3) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      expand_loop_continue_here ();
		      if ($10) cplus_expand_expr_stmt ($10);
		      expand_end_loop ();
		    }
		  pop_momentary ();
		  if (flag_new_for_scope > 0)
		    {
		      do_poplevel ();
		    }
		  finish_stmt (); }
	| SWITCH .pushlevel '(' condition ')'
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (SWITCH_STMT, $4, NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      c_expand_start_case ($4);
		    }
		  push_switch ();
		  /* Don't let the tree nodes for $4 be discarded by
		     clear_momentary during the parsing of the next stmt.  */
		  push_momentary ();
		}
	  implicitly_scoped_stmt
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>6, 1) = TREE_CHAIN ($<ttype>6);
		      TREE_CHAIN ($<ttype>6) = NULL_TREE;
		      last_tree = $<ttype>6;
		    }
		  else
		    expand_end_case ($4);
		  pop_momentary ();
		  pop_switch (); 
		}
	  .poplevel
		{ finish_stmt (); }
	| CASE expr_no_commas ':'
		{ do_case ($2, NULL_TREE); }
	  stmt
	| CASE expr_no_commas ELLIPSIS expr_no_commas ':'
		{ do_case ($2, $4); }
	  stmt
	| DEFAULT ':'
		{ do_case (NULL_TREE, NULL_TREE); }
	  stmt
	| BREAK ';'
		{ emit_line_note (input_filename, lineno);
		  if (processing_template_decl)
		    add_tree (build_min_nt (BREAK_STMT));
		  else if ( ! expand_exit_something ())
		    error ("break statement not within loop or switch"); }
	| CONTINUE ';'
		{ emit_line_note (input_filename, lineno);
		  if (processing_template_decl)
		    add_tree (build_min_nt (CONTINUE_STMT));
		  else if (! expand_continue_loop (0))
		    error ("continue statement not within a loop"); }
	| RETURN ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return (NULL_TREE); }
	| RETURN expr ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return ($2);
		  finish_stmt ();
		}
	| asm_keyword maybe_cv_qualifier '(' string ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  expand_asm ($4);
		  finish_stmt ();
		}
	/* This is the case with just output operands.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, NULL_TREE, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with input operands as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with clobbered registers as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':'
	  asm_operands ':' asm_clobbers ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, $10,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	| GOTO '*' expr ';'
		{
		  if (processing_template_decl)
		    add_tree (build_min_nt (GOTO_STMT, $3));
		  else
		    { emit_line_note (input_filename, lineno);
		      expand_computed_goto ($3); }
		}
	| GOTO identifier ';'
		{
		  if (processing_template_decl)
		    add_tree (build_min_nt (GOTO_STMT, $2));
		  else
		    {
		      tree decl;
		      emit_line_note (input_filename, lineno);
		      decl = lookup_label ($2);
		      TREE_USED (decl) = 1;
		      expand_goto (decl); 
		    }
		}
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

function_try_block:
	  TRY
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  expand_start_early_try_stmts ();
		}
	  ctor_initializer_opt compstmt
		{ expand_start_all_catch (); }
	  handler_seq
		{
		  int nested = (hack_decl_function_context
				(current_function_decl) != NULL_TREE);
		  expand_end_all_catch ();
		  finish_function (lineno, (int)$3, nested);
		}
	;

try_block:
	  TRY
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (TRY_BLOCK, NULL_TREE,
						NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		  else
		    {
		      emit_line_note (input_filename, lineno);
		      expand_start_try_stmts ();
		    }
		}
	  compstmt
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    expand_start_all_catch ();
		}
	  handler_seq
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 1) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    expand_end_all_catch ();
		}
	;

handler_seq:
	  handler
	| handler_seq handler
	;

handler:
	  CATCH
		{
		  if (processing_template_decl)
		    {
		      $<ttype>$ = build_min_nt (HANDLER, NULL_TREE,
						NULL_TREE);
		      add_tree ($<ttype>$);
		    }
		}
	  .pushlevel handler_args
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 0) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		}	  
	  compstmt
		{
		  if (processing_template_decl)
		    {
		      TREE_OPERAND ($<ttype>2, 1) = TREE_CHAIN ($<ttype>2);
		      TREE_CHAIN ($<ttype>2) = NULL_TREE;
		      last_tree = $<ttype>2;
		    }
		  else
		    expand_end_catch_block ();
		}	  
	  .poplevel
	;

type_specifier_seq:
	  typed_typespecs  %prec EMPTY
	| nonempty_cv_qualifiers  %prec EMPTY
	;

handler_args:
	  '(' ELLIPSIS ')'
		{ expand_start_catch_block (NULL_TREE, NULL_TREE); }
	/* This doesn't allow reference parameters, the below does.
	| '(' type_specifier_seq absdcl ')'
		{ check_for_new_type ("inside exception declarations", $2);
		  expand_start_catch_block ($2.t, $3); }
	| '(' type_specifier_seq ')'
		{ check_for_new_type ("inside exception declarations", $2);
		  expand_start_catch_block ($2.t, NULL_TREE); }
	| '(' type_specifier_seq notype_declarator ')'
		{ check_for_new_type ("inside exception declarations", $2);
		  expand_start_catch_block ($2.t, $3); }
	| '(' typed_typespecs after_type_declarator ')'
		{ check_for_new_type ("inside exception declarations", $2);
		  expand_start_catch_block ($2.t, $3); }
	This allows reference parameters...  */
	| '(' parm ')'
		{ check_for_new_type ("inside exception declarations", $2);
		  expand_start_catch_block (TREE_PURPOSE ($2.t),
					    TREE_VALUE ($2.t)); }
	;

label_colon:
	  IDENTIFIER ':'
		{ tree label;
		do_label:
		  label = define_label (input_filename, lineno, $1);
		  if (label && ! minimal_parse_mode)
		    expand_label (label);
		}
	| PTYPENAME ':'
		{ goto do_label; }
	| TYPENAME ':'
		{ goto do_label; }
	| SELFNAME ':'
		{ goto do_label; }
	;

for.init.statement:
	  xexpr ';'
		{ if ($1) cplus_expand_expr_stmt ($1); }
	| decl
	| '{' compstmtend
		{ if (pedantic)
		    pedwarn ("ANSI C++ forbids compound statements inside for initializations");
		}
	;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_cv_qualifier:
	  /* empty */
		{ emit_line_note (input_filename, lineno);
		  $$ = NULL_TREE; }
	| CV_QUALIFIER
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
asm_operands:
	  /* empty */
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

parmlist:
	  /* empty */
		{
		  $$ = empty_parms();
		}
	| complex_parmlist
	| type_id
		{ $$ = tree_cons (NULL_TREE, $1.t, void_list_node);
		  TREE_PARMLIST ($$) = 1; 
		  check_for_new_type ("inside parameter list", $1); }
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
		  $$ = build_tree_list (NULL_TREE, $1.t); 
		  TREE_PARMLIST ($$) = 1;
		}
	| ELLIPSIS
		{
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
		  $$ = build_tree_list (NULL_TREE, $1.t);
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
		  $$ = tree_cons (NULL_TREE, $1.t, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		  yyungetc (':', 0);
		  yychar = ')';
		}
	;

/* A default argument to a */
defarg:
	  '='
		{ maybe_snarf_defarg (); }
	  defarg1
		{ $$ = $3; }
	;

defarg1:
	  DEFARG
	| init
	;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  named_parm
		{ check_for_new_type ("in a parameter list", $1);
		  $$ = build_tree_list (NULL_TREE, $1.t); }
	| parm defarg
		{ check_for_new_type ("in a parameter list", $1);
		  $$ = build_tree_list ($2, $1.t); }
	| parms_comma full_parm
		{ check_for_new_type ("in a parameter list", $2);
		  $$ = chainon ($$, $2.t); }
	| parms_comma bad_parm
		{ $$ = chainon ($$, build_tree_list (NULL_TREE, $2)); }
	| parms_comma bad_parm '=' init
		{ $$ = chainon ($$, build_tree_list ($4, $2)); }
	;

parms_comma:
	  parms ','
	| type_id ','
		{ check_for_new_type ("in a parameter list", $1);
		  $$ = build_tree_list (NULL_TREE, $1.t); }
	;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  */
named_parm:
	/* Here we expand typed_declspecs inline to avoid mis-parsing of
	   TYPESPEC IDENTIFIER.  */
	  typed_declspecs1 declarator
		{ tree specs = strip_attrs ($1.t);
		  $$.new_type_flag = $1.new_type_flag;
		  $$.t = build_tree_list (specs, $2); }
	| typed_typespecs declarator
		{ $$.t = build_tree_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec declarator
		{ $$.t = build_tree_list (get_decl_list ($1.t), $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_declspecs1 absdcl
		{ tree specs = strip_attrs ($1.t);
		  $$.t = build_tree_list (specs, $2);
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_declspecs1  %prec EMPTY
		{ tree specs = strip_attrs ($1.t);
		  $$.t = build_tree_list (specs, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	| declmods notype_declarator
		{ tree specs = strip_attrs ($1);
		  $$.t = build_tree_list (specs, $2); 
		  $$.new_type_flag = 0; }
	;

full_parm:
	  parm
		{ $$.t = build_tree_list (NULL_TREE, $1.t);
		  $$.new_type_flag = $1.new_type_flag;  }
	| parm defarg
		{ $$.t = build_tree_list ($2, $1.t);
		  $$.new_type_flag = $1.new_type_flag;  }
	;

parm:
	  named_parm
	| type_id
	;

see_typename:
	  /* empty */  %prec EMPTY
		{ see_typename (); }
	;

bad_parm:
	  /* empty */ %prec EMPTY
		{
		  error ("type specifier omitted for parameter");
		  $$ = build_tree_list (integer_type_node, NULL_TREE);
		}
	| notype_declarator
		{
		  error ("type specifier omitted for parameter");
		  if (TREE_CODE ($$) == SCOPE_REF
		      && TREE_CODE (TREE_OPERAND ($$, 0)) == TEMPLATE_TYPE_PARM)
		    cp_error ("  perhaps you want `typename %E' to make it a type", $$);
		  $$ = build_tree_list (integer_type_node, $$);
		}
	;

exception_specification_opt:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| THROW '(' ansi_raise_identifiers  ')'  %prec EMPTY
		{ $$ = $3; }
	| THROW LEFT_RIGHT  %prec EMPTY
		{ $$ = build_decl_list (NULL_TREE, NULL_TREE); }
	;

ansi_raise_identifier:
	  type_id
		{ $$ = build_decl_list (NULL_TREE, groktypename($1.t)); }
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
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| '*' cv_qualifiers conversion_declarator
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' cv_qualifiers conversion_declarator
		{ $$ = make_reference_declarator ($2, $3); }
	| ptr_to_mem cv_qualifiers conversion_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_parse_node (SCOPE_REF, $1, arg);
		}
	;

operator:
	  OPERATOR
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
	| operator NEW  %prec EMPTY
		{ $$ = ansi_opname[NEW_EXPR]; }
	| operator DELETE  %prec EMPTY
		{ $$ = ansi_opname[DELETE_EXPR]; }
	| operator NEW '[' ']'
		{ $$ = ansi_opname[VEC_NEW_EXPR]; }
	| operator DELETE '[' ']'
		{ $$ = ansi_opname[VEC_DELETE_EXPR]; }
	/* Names here should be looked up in class scope ALSO.  */
	| operator type_specifier_seq conversion_declarator
		{ $$ = grokoptypename ($2.t, $3); }
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
