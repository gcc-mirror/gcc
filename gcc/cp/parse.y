/* YACC parser for C++ syntax.
   Copyright (C) 1988, 1989, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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

#include "system.h"

#include "tree.h"
#include "input.h"
#include "flags.h"
#include "cp-tree.h"
#include "lex.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "ggc.h"

extern struct obstack permanent_obstack;

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

#define OP0(NODE) (TREE_OPERAND (NODE, 0))
#define OP1(NODE) (TREE_OPERAND (NODE, 1))

/* Contains the statement keyword (if/while/do) to include in an
   error message if the user supplies an empty conditional expression.  */
static const char *cond_stmt_keyword;

/* Nonzero if we have an `extern "C"' acting as an extern specifier.  */
int have_extern_spec;
int used_extern_spec;

/* List of types and structure classes of the current declaration.  */
static tree current_declspecs;

/* List of prefix attributes in effect.
   Prefix attributes are parsed by the reserved_declspecs and declmods
   rules.  They create a list that contains *both* declspecs and attrs.  */
/* ??? It is not clear yet that all cases where an attribute can now appear in
   a declspec list have been updated.  */
static tree prefix_attributes;

/* When defining an enumeration, this is the type of the enumeration.  */
static tree current_enum_type;

/* When parsing a conversion operator name, this is the scope of the
   operator itself.  */
static tree saved_scopes;

static tree empty_parms PARAMS ((void));
static tree parse_decl0 PARAMS ((tree, tree, tree, tree, int));
static tree parse_decl PARAMS ((tree, tree, int));
static void parse_end_decl PARAMS ((tree, tree, tree));
static tree parse_field0 PARAMS ((tree, tree, tree, tree, tree, tree));
static tree parse_field PARAMS ((tree, tree, tree, tree));
static tree parse_bitfield0 PARAMS ((tree, tree, tree, tree, tree));
static tree parse_bitfield PARAMS ((tree, tree, tree));
static tree parse_method PARAMS ((tree, tree, tree));
static void frob_specs PARAMS ((tree, tree));
static void check_class_key PARAMS ((tree, tree));

/* Cons up an empty parameter list.  */
static inline tree
empty_parms ()
{
  tree parms;

#ifndef NO_IMPLICIT_EXTERN_C
  if (in_system_header && current_class_type == NULL 
      && current_lang_name == lang_name_c)
    parms = NULL_TREE;
  else
#endif
  parms = void_list_node;
  return parms;
}

/* Record the decl-specifiers, attributes and type lookups from the
   decl-specifier-seq in a declaration.  */

static void
frob_specs (specs_attrs, lookups)
     tree specs_attrs, lookups;
{
  save_type_access_control (lookups);
  split_specs_attrs (specs_attrs, &current_declspecs, &prefix_attributes);
  if (current_declspecs
      && TREE_CODE (current_declspecs) != TREE_LIST)
    current_declspecs = build_tree_list (NULL_TREE, current_declspecs);
  if (have_extern_spec && !used_extern_spec)
    {
      /* We have to indicate that there is an "extern", but that it
         was part of a language specifier.  For instance,
	 
    	    extern "C" typedef int (*Ptr) ();

         is well formed.  */
      current_declspecs = tree_cons (error_mark_node,
				     get_identifier ("extern"), 
				     current_declspecs);
      used_extern_spec = 1;
    }
}

static tree
parse_decl (declarator, attributes, initialized)
     tree declarator, attributes;
     int initialized;
{
  return start_decl (declarator, current_declspecs, initialized,
		     attributes, prefix_attributes);
}

static tree
parse_decl0 (declarator, specs_attrs, lookups, attributes, initialized)
     tree declarator, specs_attrs, lookups, attributes;
     int initialized;
{
  frob_specs (specs_attrs, lookups);
  return parse_decl (declarator, attributes, initialized);
}

static void
parse_end_decl (decl, init, asmspec)
     tree decl, init, asmspec;
{
  /* If decl is NULL_TREE, then this was a variable declaration using
     () syntax for the initializer, so we handled it in grokdeclarator.  */
  if (decl)
    decl_type_access_control (decl);
  cp_finish_decl (decl, init, asmspec, init ? LOOKUP_ONLYCONVERTING : 0);
}

static tree
parse_field (declarator, attributes, asmspec, init)
     tree declarator, attributes, asmspec, init;
{
  tree d = grokfield (declarator, current_declspecs, init, asmspec,
		      chainon (attributes, prefix_attributes));
  decl_type_access_control (d);
  return d;
}

static tree
parse_field0 (declarator, specs_attrs, lookups, attributes, asmspec, init)
     tree declarator, specs_attrs, lookups, attributes, asmspec, init;
{
  frob_specs (specs_attrs, lookups);
  return parse_field (declarator, attributes, asmspec, init);
}

static tree
parse_bitfield (declarator, attributes, width)
     tree declarator, attributes, width;
{
  tree d = grokbitfield (declarator, current_declspecs, width);
  cplus_decl_attributes (&d, chainon (attributes, prefix_attributes), 0);
  decl_type_access_control (d);
  return d;
}

static tree
parse_bitfield0 (declarator, specs_attrs, lookups, attributes, width)
     tree declarator, specs_attrs, lookups, attributes, width;
{
  frob_specs (specs_attrs, lookups);
  return parse_bitfield (declarator, attributes, width);
}

static tree
parse_method (declarator, specs_attrs, lookups)
     tree declarator, specs_attrs, lookups;
{
  tree d;
  frob_specs (specs_attrs, lookups);
  d = start_method (current_declspecs, declarator, prefix_attributes);
  decl_type_access_control (d);
  return d;
}

static void
check_class_key (key, aggr)
     tree key;
     tree aggr;
{
  if (TREE_CODE (key) == TREE_LIST)
    key = TREE_VALUE (key);
  if ((key == union_type_node) != (TREE_CODE (aggr) == UNION_TYPE))
    pedwarn ("`%s' tag used in naming `%#T'",
	     key == union_type_node ? "union"
	     : key == record_type_node ? "struct" : "class", aggr);
}

void
cp_parse_init ()
{
  ggc_add_tree_root (&current_declspecs, 1);
  ggc_add_tree_root (&prefix_attributes, 1);
  ggc_add_tree_root (&current_enum_type, 1);
  ggc_add_tree_root (&saved_scopes, 1);
}

/* Rename the "yyparse" function so that we can override it elsewhere.  */
#define yyparse yyparse_1
%}

%start program

%union {
  long itype; 
  tree ttype; 
  char *strtype; 
  enum tree_code code; 
  flagged_type_tree ftype;
  struct unparsed_text *pi;
}

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

/* __func__, __FUNCTION__ or __PRETTY_FUNCTION__.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token VAR_FUNC_NAME

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else.  */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN_KEYWORD GOTO ASM_KEYWORD TYPEOF ALIGNOF
%token SIGOF
%token ATTRIBUTE EXTENSION LABEL
%token REALPART IMAGPART VA_ARG

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <ttype> VISSPEC
%token DELETE NEW THIS OPERATOR CXX_TRUE CXX_FALSE
%token NAMESPACE TYPENAME_KEYWORD USING
%token LEFT_RIGHT TEMPLATE
%token TYPEID DYNAMIC_CAST STATIC_CAST REINTERPRET_CAST CONST_CAST
%token SCOPE EXPORT

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%left EMPTY			/* used to resolve s/r with epsilon */

%left error

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

%left IDENTIFIER PFUNCNAME TYPENAME SELFNAME PTYPENAME SCSPEC TYPESPEC CV_QUALIFIER ENUM AGGR ELLIPSIS TYPEOF SIGOF OPERATOR NSNAME TYPENAME_KEYWORD ATTRIBUTE

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
%left <ttype> LEFT_RIGHT
%left <code> POINTSAT '.' '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE TRY CATCH

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> PFUNCNAME maybe_identifier
%type <ttype> paren_expr_or_null nontrivial_exprlist SELFNAME
%type <ttype> expr_no_commas expr_no_comma_rangle
%type <ttype> cast_expr unary_expr primary string STRING
%type <ttype> reserved_declspecs boolean.literal
%type <ttype> reserved_typespecquals
%type <ttype> SCSPEC TYPESPEC CV_QUALIFIER maybe_cv_qualifier
%type <ttype> init initlist maybeasm maybe_init defarg defarg1
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attributes attribute attribute_list attrib
%type <ttype> any_word

%type <itype> save_lineno
%type <ttype> simple_stmt simple_if

%type <ttype> declarator notype_declarator after_type_declarator
%type <ttype> notype_declarator_intern absdcl_intern
%type <ttype> after_type_declarator_intern
%type <ttype> direct_notype_declarator direct_after_type_declarator
%type <itype> components notype_components
%type <ttype> component_decl component_decl_1 
%type <ttype> component_declarator component_declarator0
%type <ttype> notype_component_declarator notype_component_declarator0
%type <ttype> after_type_component_declarator after_type_component_declarator0
%type <ttype> absdcl cv_qualifiers
%type <ttype> direct_abstract_declarator conversion_declarator
%type <ttype> new_declarator direct_new_declarator
%type <ttype> xexpr parmlist parms bad_parm 
%type <ttype> identifiers_or_typenames
%type <ttype> fcast_or_absdcl regcast_or_absdcl
%type <ttype> expr_or_declarator expr_or_declarator_intern
%type <ttype> complex_notype_declarator
%type <ttype> notype_unqualified_id unqualified_id qualified_id
%type <ttype> template_id do_id object_template_id notype_template_declarator
%type <ttype> overqualified_id notype_qualified_id any_id
%type <ttype> complex_direct_notype_declarator functional_cast
%type <ttype> complex_parmlist parms_comma 
%type <ttype> namespace_qualifier namespace_using_decl

%type <ftype> type_id new_type_id typed_typespecs typespec typed_declspecs
%type <ftype> typed_declspecs1 type_specifier_seq nonempty_cv_qualifiers
%type <ftype> structsp typespecqual_reserved parm named_parm full_parm
%type <ftype> declmods

%type <itype> extension

/* C++ extensions */
%token <ttype> PTYPENAME
%token <ttype> EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL DEFARG DEFARG_MARKER
%token <pi> PRE_PARSED_FUNCTION_DECL 
%type <ttype> component_constructor_declarator
%type <ttype> fn.def2 return_id constructor_declarator
%type <ttype> .begin_function_body
%type <ttype> class_head class_head_apparent_template
%type <ftype> class_head_decl class_head_defn
%type <ttype> base_class_list
%type <ttype> base_class_access_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> exception_specification_opt ansi_raise_identifier ansi_raise_identifiers
%type <ttype> operator_name
%type <ttype> object aggr
%type <itype> new delete
/* %type <ttype> primary_no_id */
%type <ttype> maybe_parmlist
%type <ttype> member_init
%type <ftype> member_init_list
%type <ttype> template_parm_header template_spec_header template_header
%type <ttype> template_parm_list template_parm
%type <ttype> template_type_parm template_template_parm
%type <code>  template_close_bracket
%type <ttype> apparent_template_type
%type <ttype> template_type template_arg_list template_arg_list_opt
%type <ttype> template_arg
%type <ttype> condition xcond paren_cond_or_null
%type <ttype> type_name nested_name_specifier nested_type ptr_to_mem
%type <ttype> complete_type_name notype_identifier nonnested_type
%type <ttype> complex_type_name nested_name_specifier_1
%type <ttype> new_initializer new_placement
%type <ttype> using_decl
%type <ttype> typename_sub typename_sub0 typename_sub1 typename_sub2
%type <ttype> explicit_template_type
/* in order to recognize aggr tags as defining and thus shadowing.  */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN
%type <ttype> handler_args
%type <ttype> self_template_type .finish_template_type

%token NSNAME
%type <ttype> NSNAME

/* Used in lex.c for parsing pragmas.  */
%token END_OF_LINE

/* lex.c and pt.c depend on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT

%{
/* Tell yyparse how to print a token's value, if yydebug is set.  */
#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
extern void yyprint			PARAMS ((FILE *, int, YYSTYPE));
%}

%%
program:
	  /* empty */
               { finish_translation_unit (); }
	| extdefs
               { finish_translation_unit (); }
	;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0.  */

extdefs:
		{ $<ttype>$ = NULL_TREE; }
	  lang_extdef
		{ $<ttype>$ = NULL_TREE; ggc_collect (); }
	| extdefs lang_extdef
		{ $<ttype>$ = NULL_TREE; ggc_collect (); }
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
		{ $$ = pedantic;
		  pedantic = 0; }
	;

asm_keyword:
	  ASM_KEYWORD
	;

lang_extdef:
		{ if (pending_lang_change) do_pending_lang_change();
		  type_lookups = NULL_TREE; }
	  extdef
		{ if (! toplevel_bindings_p ())
		  pop_everything (); }
	;

extdef:
	  fndef eat_saved_input
		{ do_pending_inlines (); }
	| datadef
		{ do_pending_inlines (); }

	| EXPORT
		{ warning ("keyword `export' not implemented, and will be ignored"); }
	  template_def
		{ do_pending_inlines (); }
	| template_def
		{ do_pending_inlines (); }
	| asm_keyword '(' string ')' ';'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3);
		  assemble_asm ($3); }
	| extern_lang_string '{' extdefs_opt '}'
		{ pop_lang_context (); }
	| extern_lang_string .hush_warning fndef .warning_ok eat_saved_input
		{ do_pending_inlines (); pop_lang_context (); }
	| extern_lang_string .hush_warning datadef .warning_ok
		{ do_pending_inlines (); pop_lang_context (); }
	| NAMESPACE identifier '{'
		{ push_namespace ($2); }
	  extdefs_opt '}'
		{ pop_namespace (); }
	| NAMESPACE '{'
		{ push_namespace (NULL_TREE); }
	  extdefs_opt '}'
		{ pop_namespace (); }
	| namespace_alias
	| using_decl ';'
		{ do_toplevel_using_decl ($1); }
	| using_directive
	| extension extdef
		{ pedantic = $1; }
	;

namespace_alias:
          NAMESPACE identifier '=' 
                { begin_only_namespace_names (); }
          any_id ';'
		{
		  end_only_namespace_names ();
		  if (lastiddecl)
		    $5 = lastiddecl;
		  do_namespace_alias ($2, $5);
		}
	;

using_decl:
	  USING qualified_id
		{ $$ = $2; }
	| USING global_scope qualified_id
		{ $$ = $3; }
	| USING global_scope unqualified_id
		{ $$ = $3; }
	;

namespace_using_decl:
	  USING namespace_qualifier identifier
	        { $$ = build_nt (SCOPE_REF, $2, $3); }
	| USING global_scope identifier
	        { $$ = build_nt (SCOPE_REF, global_namespace, $3); }
	| USING global_scope namespace_qualifier identifier
	        { $$ = build_nt (SCOPE_REF, $3, $4); }
	;

using_directive:
	  USING NAMESPACE
		{ begin_only_namespace_names (); }
	  any_id ';'
		{
		  end_only_namespace_names ();
		  /* If no declaration was found, the using-directive is
		     invalid. Since that was not reported, we need the
		     identifier for the error message. */
		  if (TREE_CODE ($4) == IDENTIFIER_NODE && lastiddecl)
		    $4 = lastiddecl;
		  do_using_directive ($4);
		}
	;

namespace_qualifier:
	  NSNAME SCOPE
		{
		  if (TREE_CODE ($$) == IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$;
		}
	| namespace_qualifier NSNAME SCOPE
		{
		  $$ = $2;
		  if (TREE_CODE ($$) == IDENTIFIER_NODE)
		    $$ = lastiddecl;
		  got_scope = $$;
		}
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
		    error ("use of linkage spec `%D' is different from previous spec `%D'", $2, current_lang_name);
		  pop_lang_context (); push_lang_context ($2); }
	;

template_parm_header:
	  TEMPLATE '<'
		{ begin_template_parm_list (); }
	  template_parm_list '>'
		{ $$ = end_template_parm_list ($4); }
	;

template_spec_header:
	  TEMPLATE '<' '>'
                { begin_specialization(); 
		  $$ = NULL_TREE; }
	;

template_header:
	  template_parm_header
	| template_spec_header
	;

template_parm_list:
	  template_parm
		{ $$ = process_template_parm (NULL_TREE, $1); }
	| template_parm_list ',' template_parm
		{ $$ = process_template_parm ($1, $3); }
	;

maybe_identifier:
	  identifier
	  	{ $$ = $1; }
	|	/* empty */
		{ $$ = NULL_TREE; }
        ;

template_type_parm:
	  aggr maybe_identifier
                { $$ = finish_template_type_parm ($1, $2); }
	| TYPENAME_KEYWORD maybe_identifier
                { $$ = finish_template_type_parm (class_type_node, $2); }
	;

template_template_parm:
	  template_parm_header aggr maybe_identifier
                { $$ = finish_template_template_parm ($2, $3); }
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
	| parm '=' expr_no_comma_rangle
		{ $$ = build_tree_list ($3, $1.t); }
	| template_template_parm
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| template_template_parm '=' template_arg
		{
		  if (TREE_CODE ($3) != TEMPLATE_DECL
		      && TREE_CODE ($3) != TEMPLATE_TEMPLATE_PARM
		      && TREE_CODE ($3) != TYPE_DECL
		      && TREE_CODE ($3) != UNBOUND_CLASS_TEMPLATE)
		    {
		      error ("invalid default template argument");
		      $3 = error_mark_node;
		    }
		  $$ = build_tree_list ($3, $1);
		}
	;

template_def:
	  template_header template_extdef
                { finish_template_decl ($1); }
	| template_header error  %prec EMPTY
                { finish_template_decl ($1); }
	;

template_extdef:
	  fndef eat_saved_input
		{ do_pending_inlines (); }
	| template_datadef
		{ do_pending_inlines (); }
	| template_def
		{ do_pending_inlines (); }
	| extern_lang_string .hush_warning fndef .warning_ok eat_saved_input
		{ do_pending_inlines ();
		  pop_lang_context (); }
	| extern_lang_string .hush_warning template_datadef .warning_ok
		{ do_pending_inlines ();
		  pop_lang_context (); }
	| extension template_extdef
		{ pedantic = $1; }
	;

template_datadef:
	  nomods_initdecls ';'
	| declmods notype_initdecls ';'
		{}
	| typed_declspecs initdecls ';'
                { note_list_got_semicolon ($1.t); }
	| structsp ';'
                {
		  if ($1.t != error_mark_node)
                    {
		      maybe_process_partial_specialization ($1.t);
		      note_got_semicolon ($1.t);
	            }
                }
	;

datadef:
	  nomods_initdecls ';'
	| declmods notype_initdecls ';'
		{}
	| typed_declspecs initdecls ';'
                { note_list_got_semicolon ($1.t); }
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
	| error END_OF_SAVED_INPUT
		{ end_input (); }
	| ';'
	| bad_decl
	;

ctor_initializer_opt:
	  nodecls
	| base_init
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

/* The outermost block of a function really begins before the
   mem-initializer-list, so we open one there and suppress the one that
   actually corresponds to the curly braces.  */
function_body:
	  .begin_function_body ctor_initializer_opt save_lineno '{'
		{ $<ttype>$ = begin_compound_stmt (/*has_no_scope=*/1); }
	  compstmtend 
                {
		  STMT_LINENO ($<ttype>5) = $3;
		  finish_compound_stmt (/*has_no_scope=*/1, $<ttype>5);
		  finish_function_body ($1);
		}
	;

fndef:
	  fn.def1 maybe_return_init function_body
		{ expand_body (finish_function (0)); }
	| fn.def1 maybe_return_init function_try_block
		{ expand_body (finish_function (0)); }
	| fn.def1 maybe_return_init error
		{ }
	;

constructor_declarator:
	  nested_name_specifier SELFNAME '(' 
                { $$ = begin_constructor_declarator ($1, $2); }
	  parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>4, $5, $7, $8); }
	| nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
                { $$ = begin_constructor_declarator ($1, $2); 
		  $$ = make_call_declarator ($$, empty_parms (), $4, $5);
		}
	| global_scope nested_name_specifier SELFNAME '(' 
                { $$ = begin_constructor_declarator ($2, $3); }
	 parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>5, $6, $8, $9); }
	| global_scope nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = begin_constructor_declarator ($2, $3);
		  $$ = make_call_declarator ($$, empty_parms (), $5, $6);
		}
	| nested_name_specifier self_template_type '(' 
                { $$ = begin_constructor_declarator ($1, $2); }
	  parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>4, $5, $7, $8); }
	| nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = begin_constructor_declarator ($1, $2);
		  $$ = make_call_declarator ($$, empty_parms (), $4, $5);
		}
	| global_scope nested_name_specifier self_template_type '(' 
                { $$ = begin_constructor_declarator ($2, $3); }
	 parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($<ttype>5, $6, $8, $9); }
	| global_scope nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = begin_constructor_declarator ($2, $3); 
		  $$ = make_call_declarator ($$, empty_parms (), $5, $6);
		}
	;

fn.def1:
	  typed_declspecs declarator
		{ check_for_new_type ("return type", $1);
		  if (!begin_function_definition ($1.t, $2))
		    YYERROR1; }
	| declmods notype_declarator
		{ if (!begin_function_definition ($1.t, $2))
		    YYERROR1; }
	| notype_declarator
		{ if (!begin_function_definition (NULL_TREE, $1))
		    YYERROR1; }
	| declmods constructor_declarator
		{ if (!begin_function_definition ($1.t, $2))
		    YYERROR1; }
	| constructor_declarator
		{ if (!begin_function_definition (NULL_TREE, $1))
		    YYERROR1; }
	;

/* ANSI allows optional parentheses around constructor class names.
   See ISO/IEC 14882:1998(E) 12.1.  */

component_constructor_declarator:
          SELFNAME '(' parmlist ')' cv_qualifiers exception_specification_opt
                { $$ = make_call_declarator ($1, $3, $5, $6); }
        | '(' SELFNAME ')' '(' parmlist ')' cv_qualifiers
                exception_specification_opt
                { $$ = make_call_declarator ($2, $5, $7, $8); }
        | SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
                { $$ = make_call_declarator ($1, empty_parms (), $3, $4); }
        | '(' SELFNAME ')' LEFT_RIGHT cv_qualifiers exception_specification_opt
                { $$ = make_call_declarator ($2, empty_parms (), $5, $6); }
	| self_template_type '(' parmlist ')' cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, $3, $5, $6); }
	| self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
		{ $$ = make_call_declarator ($1, empty_parms (), $3, $4); }
	;

/* more C++ complexity.  See component_decl for a comment on the
   reduce/reduce conflict introduced by these rules.  */
fn.def2:
	  declmods component_constructor_declarator
		{ $$ = parse_method ($2, $1.t, $1.lookups);
		 rest_of_mdef:
		  if (! $$)
		    YYERROR1;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  snarf_method ($$); }
	| component_constructor_declarator
		{ $$ = parse_method ($1, NULL_TREE, NULL_TREE); 
		  goto rest_of_mdef; }
	| typed_declspecs declarator
		{ $$ = parse_method ($2, $1.t, $1.lookups); goto rest_of_mdef;}
	| declmods notype_declarator
		{ $$ = parse_method ($2, $1.t, $1.lookups); goto rest_of_mdef;}
	| notype_declarator
		{ $$ = parse_method ($1, NULL_TREE, NULL_TREE); 
		  goto rest_of_mdef; }
	| declmods constructor_declarator
		{ $$ = parse_method ($2, $1.t, $1.lookups); goto rest_of_mdef;}
	| constructor_declarator
		{ $$ = parse_method ($1, NULL_TREE, NULL_TREE); 
		  goto rest_of_mdef; }
	;

return_id:
	  RETURN_KEYWORD IDENTIFIER
		{
		  $$ = $2;
		}
	;

return_init:
	  return_id maybe_init
		{ finish_named_return_value ($<ttype>$, $2); }
	| return_id '(' nonnull_exprlist ')'
		{ finish_named_return_value ($<ttype>$, $3); }
	| return_id LEFT_RIGHT
		{ finish_named_return_value ($<ttype>$, NULL_TREE); }
	;

base_init:
	  ':' member_init_list
		{
		  if (! DECL_CONSTRUCTOR_P (current_function_decl))
		    error ("only constructors take base initializers");
		  else if ($2.new_type_flag == 0)
		    error ("no base or member initializers given following ':'");

		  finish_mem_initializers ($2.t);
		}
	;

.begin_function_body:
	  /* empty */
		{
		  $$ = begin_function_body ();
		}
	;

member_init_list:
	  /* empty */
		{ 
		  $$.new_type_flag = 0; 
		  $$.t = NULL_TREE; 
		}
	| member_init
		{ 
		  $$.new_type_flag = 1; 
		  $$.t = $1; 
		}
	| member_init_list ',' member_init
                { 
		  if ($3) 
		    {
		      $$.new_type_flag = 1; 
		      TREE_CHAIN ($3) = $1.t;
		      $$.t = $3;
		    }
		  else
		    $$ = $1;
		}
	| member_init_list error
	;

member_init:
	  '(' nonnull_exprlist ')'
		{
		  if (current_class_name)
		    pedwarn ("anachronistic old style base class initializer");
		  $$ = expand_member_init (current_class_ref, NULL_TREE, $2);
		}
	| LEFT_RIGHT
		{
		  if (current_class_name)
		    pedwarn ("anachronistic old style base class initializer");
		  $$ = expand_member_init (current_class_ref,
					   NULL_TREE, 
					   void_type_node);
		}
	| notype_identifier '(' nonnull_exprlist ')'
		{ $$ = expand_member_init (current_class_ref, $1, $3); }
	| notype_identifier LEFT_RIGHT
		{ $$ = expand_member_init (current_class_ref, $1,
					   void_type_node); }
	| nonnested_type '(' nonnull_exprlist ')'
		{ $$ = expand_member_init (current_class_ref, $1, $3); }
	| nonnested_type LEFT_RIGHT
		{ $$ = expand_member_init (current_class_ref, $1,
					   void_type_node); }
	| typename_sub '(' nonnull_exprlist ')'
		{ $$ = expand_member_init (current_class_ref, $1, $3); }
	| typename_sub LEFT_RIGHT
		{ $$ = expand_member_init (current_class_ref, $1,
					   void_type_node); }
        | error
                { $$ = NULL_TREE; }
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
	  TEMPLATE begin_explicit_instantiation typespec ';'
		{ do_type_instantiation ($3.t, NULL_TREE, 1);
		  yyungetc (';', 1); }
          end_explicit_instantiation
	| TEMPLATE begin_explicit_instantiation typed_declspecs declarator
		{ tree specs = strip_attrs ($3.t);
		  do_decl_instantiation (specs, $4, NULL_TREE); }
          end_explicit_instantiation
	| TEMPLATE begin_explicit_instantiation notype_declarator
		{ do_decl_instantiation (NULL_TREE, $3, NULL_TREE); }
          end_explicit_instantiation
	| TEMPLATE begin_explicit_instantiation constructor_declarator
		{ do_decl_instantiation (NULL_TREE, $3, NULL_TREE); }
          end_explicit_instantiation
	| SCSPEC TEMPLATE begin_explicit_instantiation typespec ';'
		{ do_type_instantiation ($4.t, $1, 1);
		  yyungetc (';', 1); }
          end_explicit_instantiation
	| SCSPEC TEMPLATE begin_explicit_instantiation typed_declspecs 
          declarator
		{ tree specs = strip_attrs ($4.t);
		  do_decl_instantiation (specs, $5, $1); }
          end_explicit_instantiation
	| SCSPEC TEMPLATE begin_explicit_instantiation notype_declarator
		{ do_decl_instantiation (NULL_TREE, $4, $1); }
          end_explicit_instantiation
	| SCSPEC TEMPLATE begin_explicit_instantiation constructor_declarator
		{ do_decl_instantiation (NULL_TREE, $4, $1); }
          end_explicit_instantiation
	;

begin_explicit_instantiation: 
      { begin_explicit_instantiation(); }
        ;

end_explicit_instantiation: 
      { end_explicit_instantiation(); }
        ;

/* The TYPENAME expansions are to deal with use of a template class name as
  a template within the class itself, where the template decl is hidden by
  a type decl.  Got all that?  */

template_type:
	  PTYPENAME '<' template_arg_list_opt template_close_bracket
	    .finish_template_type
                { $$ = $5; }
	| TYPENAME  '<' template_arg_list_opt template_close_bracket
	    .finish_template_type
                { $$ = $5; }
	| self_template_type
	;

apparent_template_type:
	  template_type
	| identifier '<' template_arg_list_opt '>'
	    .finish_template_type
		{ $$ = $5; }
        ;

self_template_type:
	  SELFNAME  '<' template_arg_list_opt template_close_bracket
	    .finish_template_type
                { $$ = $5; }
	;

.finish_template_type:
                { 
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;

		  $$ = finish_template_type ($<ttype>-3, $<ttype>-1, 
					     yychar == SCOPE);
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

template_arg_list_opt:
         /* empty */
                 { $$ = NULL_TREE; }
       | template_arg_list
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
	| PTYPENAME
		{
		  $$ = lastiddecl;
		  if (DECL_TEMPLATE_TEMPLATE_PARM_P ($$))
		    $$ = TREE_TYPE ($$);
		}
	| global_scope PTYPENAME
		{
		  $$ = lastiddecl;
		  if (DECL_TEMPLATE_TEMPLATE_PARM_P ($$))
		    $$ = TREE_TYPE ($$);
		}
	| expr_no_comma_rangle
	| nested_name_specifier TEMPLATE identifier
		{
		  if (!processing_template_decl)
		    {
		      error ("use of template qualifier outside template");
		      $$ = error_mark_node;
		    }
		  else
		    $$ = make_unbound_class_template ($1, $3, 1);
		}
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
		{ error ("ISO C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' expr ')'
                { $$ = $2; }
	;

paren_cond_or_null:
	LEFT_RIGHT
		{ error ("ISO C++ forbids an empty condition for `%s'",
			 cond_stmt_keyword);
		  $$ = integer_zero_node; }
	| '(' condition ')'
                { $$ = $2; }
	;

xcond:
	  /* empty */
		{ $$ = NULL_TREE; }
	| condition
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
			error ("definition of class `%T' in condition", s);
		      else if (TREE_CODE (s) == ENUMERAL_TYPE)
			error ("definition of enum `%T' in condition", s);
		    }
		  }
		  current_declspecs = $1.t;
		  $<ttype>$ = parse_decl ($<ttype>2, $4, 1);
		}
	  init
		{ 
		  parse_end_decl ($<ttype>6, $7, $4);
		  $$ = convert_from_reference ($<ttype>6); 
		  if (TREE_CODE (TREE_TYPE ($$)) == ARRAY_TYPE)
		    error ("definition of array `%#D' in condition", $$); 
		}
	| expr
	;

compstmtend:
	  '}'
	| maybe_label_decls stmts '}'
	| maybe_label_decls stmts error '}'
	| maybe_label_decls error '}'
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
	  primary  %prec UNARY
		{ $$ = $1; }
	/* __extension__ turns off -pedantic for following primary.  */
	| extension cast_expr  	  %prec UNARY
		{ $$ = $2;
		  pedantic = $1; }
	| '*' cast_expr   %prec UNARY
		{ $$ = build_x_indirect_ref ($2, "unary *"); }
	| '&' cast_expr   %prec UNARY
		{ $$ = build_x_unary_op (ADDR_EXPR, $2); }
	| '~' cast_expr
		{ $$ = build_x_unary_op (BIT_NOT_EXPR, $2); }
	| unop cast_expr  %prec UNARY
                { $$ = finish_unary_op_expr ($1, $2); }
	/* Refer to the address of a label as a pointer.  */
	| ANDAND identifier
		{ $$ = finish_label_address_expr ($2); }
	| sizeof unary_expr  %prec UNARY
		{ $$ = finish_sizeof ($2);
		  skip_evaluation--; }
	| sizeof '(' type_id ')'  %prec HYPERUNARY
		{ $$ = finish_sizeof (groktypename ($3.t));
		  check_for_new_type ("sizeof", $3);
		  skip_evaluation--; }
	| alignof unary_expr  %prec UNARY
		{ $$ = finish_alignof ($2);
		  skip_evaluation--; }
	| alignof '(' type_id ')'  %prec HYPERUNARY
		{ $$ = finish_alignof (groktypename ($3.t)); 
		  check_for_new_type ("alignof", $3);
		  skip_evaluation--; }

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
	| new '(' type_id ')'
            %prec EMPTY
		{ $$ = build_new (NULL_TREE, groktypename($3.t),
				  NULL_TREE, $1); 
		  check_for_new_type ("new", $3); }
	| new '(' type_id ')' new_initializer
		{ $$ = build_new (NULL_TREE, groktypename($3.t), $5, $1); 
		  check_for_new_type ("new", $3); }
	| new new_placement '(' type_id ')' %prec EMPTY
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
                { pedwarn ("old style placement syntax, use () instead");
		  $$ = $2; }
	;

new_initializer:
	  '(' nonnull_exprlist ')'
		{ $$ = $2; }
	| LEFT_RIGHT
		{ $$ = void_zero_node; }
	| '(' typespec ')'
		{
		  error ("`%T' is not a valid expression", $2.t);
		  $$ = error_mark_node;
		}
	/* GNU extension so people can use initializer lists.  Note that
	   this alters the meaning of `new int = 1', which was previously
	   syntactically valid but semantically invalid.  
           This feature is now deprecated and will be removed in a future
           release.  */
	| '=' init
		{
		  if (pedantic)
		    pedwarn ("ISO C++ forbids initialization of new expression with `='");
		  cp_deprecated ("new initializer lists extension");
		  if (TREE_CODE ($2) != TREE_LIST
		      && TREE_CODE ($2) != CONSTRUCTOR)
		    $$ = build_tree_list (NULL_TREE, $2);
		  else
		    $$ = $2;
		}
	;

/* This is necessary to postpone reduction of `int ((int)(int)(int))'.  */
regcast_or_absdcl:
	  '(' type_id ')'  %prec EMPTY
		{ $2.t = finish_parmlist (build_tree_list (NULL_TREE, $2.t), 0);
		  $$ = make_call_declarator (NULL_TREE, $2.t, NULL_TREE, NULL_TREE);
		  check_for_new_type ("cast", $2); }
	| regcast_or_absdcl '(' type_id ')'  %prec EMPTY
		{ $3.t = finish_parmlist (build_tree_list (NULL_TREE, $3.t), 0); 
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
		    pedwarn ("ISO C++ forbids compound literals");
		  /* Indicate that this was a C99 compound literal.  */
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
	;

expr_no_comma_rangle:
	  cast_expr
	/* Handle general members.  */
	| expr_no_comma_rangle POINTSAT_STAR expr_no_comma_rangle
		{ $$ = build_x_binary_op (MEMBER_REF, $$, $3); }
	| expr_no_comma_rangle DOT_STAR expr_no_comma_rangle
		{ $$ = build_m_component_ref ($$, $3); }
	| expr_no_comma_rangle '+' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '-' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '*' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '/' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '%' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle LSHIFT expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle RSHIFT expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle ARITHCOMPARE expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '<' expr_no_comma_rangle
		{ $$ = build_x_binary_op (LT_EXPR, $$, $3); }
	| expr_no_comma_rangle EQCOMPARE expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle MIN_MAX expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '&' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '|' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle '^' expr_no_comma_rangle
		{ $$ = build_x_binary_op ($2, $$, $3); }
	| expr_no_comma_rangle ANDAND expr_no_comma_rangle
		{ $$ = build_x_binary_op (TRUTH_ANDIF_EXPR, $$, $3); }
	| expr_no_comma_rangle OROR expr_no_comma_rangle
		{ $$ = build_x_binary_op (TRUTH_ORIF_EXPR, $$, $3); }
	| expr_no_comma_rangle '?' xexpr ':' expr_no_comma_rangle
		{ $$ = build_x_conditional_expr ($$, $3, $5); }
	| expr_no_comma_rangle '=' expr_no_comma_rangle
		{ $$ = build_x_modify_expr ($$, NOP_EXPR, $3);
		  if ($$ != error_mark_node)
                    C_SET_EXP_ORIGINAL_CODE ($$, MODIFY_EXPR); }
	| expr_no_comma_rangle ASSIGN expr_no_comma_rangle
		{ $$ = build_x_modify_expr ($$, $2, $3); }
	| THROW
		{ $$ = build_throw (NULL_TREE); }
	| THROW expr_no_comma_rangle
		{ $$ = build_throw ($2); }
	;

notype_unqualified_id:
	  '~' see_typename identifier
		{ $$ = build_nt (BIT_NOT_EXPR, $3); }
	| '~' see_typename template_type
		{ $$ = build_nt (BIT_NOT_EXPR, $3); }
        | template_id
	| operator_name
	| IDENTIFIER
	| PTYPENAME
	| NSNAME  %prec EMPTY
	;

do_id:
		{
		  /* If lastiddecl is a TREE_LIST, it's a baselink, which
		     means that we're in an expression like S::f<int>, so
		     don't do_identifier; we only do that for unqualified
		     identifiers.  */
	          if (!lastiddecl || TREE_CODE (lastiddecl) != TREE_LIST)
		    $$ = do_identifier ($<ttype>-1, 1, NULL_TREE);
		  else
		    $$ = $<ttype>-1;
		}
        ;

template_id:
          PFUNCNAME '<' do_id template_arg_list_opt template_close_bracket 
                { $$ = lookup_template_function ($3, $4); }
        | operator_name '<' do_id template_arg_list_opt template_close_bracket
                { $$ = lookup_template_function ($3, $4); }
	;

object_template_id:
        TEMPLATE identifier '<' template_arg_list_opt template_close_bracket
                { $$ = lookup_template_function ($2, $4); }
        | TEMPLATE PFUNCNAME '<' template_arg_list_opt template_close_bracket
                { $$ = lookup_template_function ($2, $4); }
        | TEMPLATE operator_name '<' template_arg_list_opt 
          template_close_bracket
                { $$ = lookup_template_function ($2, $4); }
        ;

unqualified_id:
	  notype_unqualified_id
	| TYPENAME
	| SELFNAME
	;

expr_or_declarator_intern:
	  expr_or_declarator
	| attributes expr_or_declarator
		{
		  /* Provide support for '(' attributes '*' declarator ')'
		     etc */
		  $$ = tree_cons ($1, $2, NULL_TREE);
		}
	;

expr_or_declarator:
	  notype_unqualified_id
	| '*' expr_or_declarator_intern  %prec UNARY
		{ $$ = build_nt (INDIRECT_REF, $2); }
	| '&' expr_or_declarator_intern  %prec UNARY
		{ $$ = build_nt (ADDR_EXPR, $2); }
	| '(' expr_or_declarator_intern ')'
		{ $$ = $2; }
	;

notype_template_declarator:
	  IDENTIFIER '<' template_arg_list_opt template_close_bracket
                { $$ = lookup_template_function ($1, $3); }
	| NSNAME '<' template_arg_list template_close_bracket
                { $$ = lookup_template_function ($1, $3); }
	;
		
direct_notype_declarator:
	  complex_direct_notype_declarator
	/* This precedence declaration is to prefer this reduce
	   to the Koenig lookup shift in primary, below.  I hate yacc.  */
	| notype_unqualified_id %prec '('
	| notype_template_declarator
	| '(' expr_or_declarator_intern ')'
                { $$ = finish_decl_parsing ($2); }
	;

primary:
	  notype_unqualified_id
		{
		  if (TREE_CODE ($1) == BIT_NOT_EXPR)
		    $$ = build_x_unary_op (BIT_NOT_EXPR, TREE_OPERAND ($1, 0));
		  else 
		    $$ = finish_id_expr ($1);
		}		
	| CONSTANT
	| boolean.literal
	| string
		{
		  $$ = combine_strings ($$);
		  /* combine_strings doesn't set up TYPE_MAIN_VARIANT of
		     a const array the way we want, so fix it.  */
		  if (flag_const_strings)
		    TREE_TYPE ($$) = build_cplus_array_type
		      (TREE_TYPE (TREE_TYPE ($$)),
		       TYPE_DOMAIN (TREE_TYPE ($$)));
		}
	| VAR_FUNC_NAME
		{
		  $$ = fname_decl (C_RID_CODE ($$), $$);
		  if (processing_template_decl)
		    $$ = build_min_nt (LOOKUP_EXPR, DECL_NAME ($$));
		}
	| '(' expr ')'
		{ $$ = finish_parenthesized_expr ($2); }
	| '(' expr_or_declarator_intern ')'
		{ $2 = reparse_decl_as_expr (NULL_TREE, $2);
		  $$ = finish_parenthesized_expr ($2); }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| '('
		{ tree scope = current_scope ();
		  if (!scope || TREE_CODE (scope) != FUNCTION_DECL)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  if (pedantic)
		    pedwarn ("ISO C++ forbids braced-groups within expressions");  
		  $<ttype>$ = begin_stmt_expr (); 
		}
	  compstmt_or_stmtexpr ')'
               { $$ = finish_stmt_expr ($<ttype>2); }
        /* Koenig lookup support
           We could store lastiddecl in $1 to avoid another lookup,
           but that would result in many additional reduce/reduce conflicts. */
        | notype_unqualified_id '(' nonnull_exprlist ')'
               { $$ = finish_call_expr ($1, $3, 1); }
        | notype_unqualified_id LEFT_RIGHT
               { $$ = finish_call_expr ($1, NULL_TREE, 1); }
	| primary '(' nonnull_exprlist ')'
               { $$ = finish_call_expr ($1, $3, 0); }
	| primary LEFT_RIGHT
               { $$ = finish_call_expr ($1, NULL_TREE, 0); }
	| VA_ARG '(' expr_no_commas ',' type_id ')'
		{ $$ = build_x_va_arg ($3, groktypename ($5.t));
		  check_for_new_type ("__builtin_va_arg", $5); }
	| primary '[' expr ']'
		{ $$ = grok_array_decl ($$, $3); }
	| primary PLUSPLUS
		{ $$ = finish_increment_expr ($1, POSTINCREMENT_EXPR); }
	| primary MINUSMINUS
		{ $$ = finish_increment_expr ($1, POSTDECREMENT_EXPR); }
	/* C++ extensions */
	| THIS
		{ $$ = finish_this_expr (); }
	| CV_QUALIFIER '(' nonnull_exprlist ')'
		{
		  /* This is a C cast in C++'s `functional' notation
		     using the "implicit int" extension so that:
		     `const (3)' is equivalent to `const int (3)'.  */
		  tree type;

		  type = hash_tree_cons (NULL_TREE, $1, NULL_TREE);
		  type = groktypename (build_tree_list (type, NULL_TREE));
		  $$ = build_functional_cast (type, $3);
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
		{ $$ = build_typeid ($3); }
	| TYPEID '(' type_id ')'
		{ tree type = groktypename ($3.t);
		  check_for_new_type ("typeid", $3);
		  $$ = get_typeid (type); }
	| global_scope IDENTIFIER
		{ $$ = do_scoped_id ($2, 1); }
	| global_scope template_id
		{ $$ = $2; }
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
                { $$ = finish_qualified_call_expr ($1, $3); }
	| overqualified_id LEFT_RIGHT
		{ $$ = finish_qualified_call_expr ($1, NULL_TREE); }
        | object object_template_id %prec UNARY
                { 
		  $$ = build_x_component_ref ($$, $2, NULL_TREE, 1); 
		}
        | object object_template_id '(' nonnull_exprlist ')'
                { $$ = finish_object_call_expr ($2, $1, $4); }
	| object object_template_id LEFT_RIGHT
                { $$ = finish_object_call_expr ($2, $1, NULL_TREE); }
	| object unqualified_id  %prec UNARY
		{ $$ = build_x_component_ref ($$, $2, NULL_TREE, 1); }
	| object overqualified_id  %prec UNARY
		{ if (processing_template_decl)
		    $$ = build_min_nt (COMPONENT_REF, $1, $2);
		  else
		    $$ = build_object_ref ($$, OP0 ($2), OP1 ($2)); }
	| object unqualified_id '(' nonnull_exprlist ')'
                { $$ = finish_object_call_expr ($2, $1, $4); }
	| object unqualified_id LEFT_RIGHT
                { $$ = finish_object_call_expr ($2, $1, NULL_TREE); }
	| object overqualified_id '(' nonnull_exprlist ')'
                { $$ = finish_qualified_object_call_expr ($2, $1, $4); }
	| object overqualified_id LEFT_RIGHT
                { $$ = finish_qualified_object_call_expr ($2, $1, NULL_TREE); }
	/* p->int::~int() is valid -- 12.4 */
	| object '~' TYPESPEC LEFT_RIGHT
		{ $$ = finish_pseudo_destructor_call_expr ($1, NULL_TREE, $3); }
	| object TYPESPEC SCOPE '~' TYPESPEC LEFT_RIGHT
		{ $$ = finish_pseudo_destructor_call_expr ($1, $2, $5); }
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
	  compstmt_or_stmtexpr ')'
		{ if (pedantic)
		    pedwarn ("ISO C++ forbids braced-groups within expressions");
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
		  if (DECL_CONSTRUCTOR_P (current_function_decl))
		    finish_mem_initializers (NULL_TREE);
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
		  if ($1.t && IS_AGGR_TYPE_CODE (TREE_CODE ($1.t)))
		    note_got_semicolon ($1.t);
		}
	| typed_declspecs initdecls ';'
		{
		  note_list_got_semicolon ($1.t);
		}
	| declmods notype_initdecls ';'
                {}
	| typed_declspecs ';'
		{
		  shadow_tag ($1.t);
		  note_list_got_semicolon ($1.t);
		}
	| declmods ';'
		{ warning ("empty declaration"); }
	| extension decl
		{ pedantic = $1; }
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

/* ISO type-id (8.1) */
type_id:
	  typed_typespecs absdcl
		{ $$.t = build_tree_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers absdcl
		{ $$.t = build_tree_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec absdcl
		{ $$.t = build_tree_list (build_tree_list (NULL_TREE, $1.t),
					  $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_typespecs  %prec EMPTY
		{ $$.t = build_tree_list ($1.t, NULL_TREE);
		  $$.new_type_flag = $1.new_type_flag;  }
	| nonempty_cv_qualifiers  %prec EMPTY
		{ $$.t = build_tree_list ($1.t, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

typed_declspecs:
	  typed_typespecs  %prec EMPTY
		{ $$.lookups = type_lookups; }
	| typed_declspecs1
		{ $$.lookups = type_lookups; }
	;

typed_declspecs1:
	  declmods typespec
		{ $$.t = tree_cons (NULL_TREE, $2.t, $1.t); 
		  $$.new_type_flag = $2.new_type_flag; }
	| typespec reserved_declspecs  %prec HYPERUNARY
		{ $$.t = tree_cons (NULL_TREE, $1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec reserved_typespecquals reserved_declspecs
		{ $$.t = tree_cons (NULL_TREE, $1.t, chainon ($2, $3)); 
		  $$.new_type_flag = $1.new_type_flag; }
	| declmods typespec reserved_declspecs
		{ $$.t = tree_cons (NULL_TREE, $2.t, chainon ($3, $1.t)); 
		  $$.new_type_flag = $2.new_type_flag; }
	| declmods typespec reserved_typespecquals
		{ $$.t = tree_cons (NULL_TREE, $2.t, chainon ($3, $1.t)); 
		  $$.new_type_flag = $2.new_type_flag; }
	| declmods typespec reserved_typespecquals reserved_declspecs
		{ $$.t = tree_cons (NULL_TREE, $2.t,
				    chainon ($3, chainon ($4, $1.t))); 
		  $$.new_type_flag = $2.new_type_flag; }
	;

reserved_declspecs:
	  SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($$));
		  $$ = build_tree_list (NULL_TREE, $$); }
	| reserved_declspecs typespecqual_reserved
		{ $$ = tree_cons (NULL_TREE, $2.t, $$); }
	| reserved_declspecs SCSPEC
		{ if (extra_warnings)
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$ = tree_cons (NULL_TREE, $2, $$); }
	;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

/* We use hash_tree_cons for lists of typeless declspecs so that they end
   up on a persistent obstack.  Otherwise, they could appear at the
   beginning of something like

      static const struct { int foo () { } } b;

   and would be discarded after we finish compiling foo.  We don't need to
   worry once we see a type.  */

declmods:
	  nonempty_cv_qualifiers  %prec EMPTY
		{ $$.lookups = NULL_TREE; TREE_STATIC ($$.t) = 1; }
	| SCSPEC
		{
		  $$.t = hash_tree_cons (NULL_TREE, $1, NULL_TREE);
		  $$.new_type_flag = 0; $$.lookups = NULL_TREE;
		}
	| declmods CV_QUALIFIER
		{
		  $$.t = hash_tree_cons (NULL_TREE, $2, $1.t);
		  TREE_STATIC ($$.t) = 1;
		}
	| declmods SCSPEC
		{
		  if (extra_warnings && TREE_STATIC ($$.t))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER ($2));
		  $$.t = hash_tree_cons (NULL_TREE, $2, $1.t);
		  TREE_STATIC ($$.t) = TREE_STATIC ($1.t);
		}
	| declmods attributes
		{ $$.t = hash_tree_cons ($2, NULL_TREE, $1.t); }
	;

/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  typespec  %prec EMPTY
		{ $$.t = build_tree_list (NULL_TREE, $1.t); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers typespec
		{ $$.t = tree_cons (NULL_TREE, $2.t, $1.t); 
		  $$.new_type_flag = $2.new_type_flag; }
	| typespec reserved_typespecquals
		{ $$.t = tree_cons (NULL_TREE, $1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| nonempty_cv_qualifiers typespec reserved_typespecquals
		{ $$.t = tree_cons (NULL_TREE, $2.t, chainon ($3, $1.t)); 
		  $$.new_type_flag = $2.new_type_flag; }
	;

reserved_typespecquals:
	  typespecqual_reserved
		{ $$ = build_tree_list (NULL_TREE, $1.t); }
	| reserved_typespecquals typespecqual_reserved
		{ $$ = tree_cons (NULL_TREE, $2.t, $1); }
	| reserved_typespecquals attributes
		{ $$ = tree_cons ($2, NULL_TREE, $1); }
	| attributes %prec EMPTY
		{ $$ = tree_cons ($1, NULL_TREE, NULL_TREE); }
	;

sizeof:
	SIZEOF { skip_evaluation++; }
	;

alignof:
	ALIGNOF { skip_evaluation++; }
	;

typeof:
	TYPEOF { skip_evaluation++; }
	;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec:
	  structsp
	  	{ $$.lookups = NULL_TREE; }
	| TYPESPEC  %prec EMPTY
		{ $$.t = $1; $$.new_type_flag = 0; $$.lookups = NULL_TREE; }
	| complete_type_name
		{ $$.t = $1; $$.new_type_flag = 0; $$.lookups = NULL_TREE; }
	| typeof '(' expr ')'
		{ $$.t = finish_typeof ($3);
		  $$.new_type_flag = 0; $$.lookups = NULL_TREE;
		  skip_evaluation--; }
	| typeof '(' type_id ')'
		{ $$.t = groktypename ($3.t);
		  $$.new_type_flag = 0; $$.lookups = NULL_TREE;
		  skip_evaluation--; }
	| SIGOF '(' expr ')'
		{ tree type = TREE_TYPE ($3);

                  $$.new_type_flag = 0; $$.lookups = NULL_TREE;
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

                  $$.new_type_flag = 0; $$.lookups = NULL_TREE;
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
            { check_multiple_declarators (); }
	;

notype_initdecls:
	  notype_initdcl0
	| notype_initdecls ',' initdcl
            { check_multiple_declarators (); }
	;

nomods_initdecls:
	  nomods_initdcl0
	| nomods_initdecls ',' initdcl
            { check_multiple_declarators (); }
	;

maybeasm:
	  /* empty */
		{ $$ = NULL_TREE; }
	| asm_keyword '(' string ')'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3); $$ = $3; }
	;

initdcl:
	  declarator maybeasm maybe_attribute '='
		{ $<ttype>$ = parse_decl ($<ttype>1, $3, 1); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ parse_end_decl ($<ttype>5, $6, $2); }
	| declarator maybeasm maybe_attribute
		{
		  $<ttype>$ = parse_decl ($<ttype>1, $3, 0);
		  parse_end_decl ($<ttype>$, NULL_TREE, $2);
		}
	;

        /* This rule assumes a certain configuration of the parser stack.
	   In particular, $0, the element directly before the beginning of
	   this rule on the stack, must be a maybeasm.  $-1 must be a
	   declarator or notype_declarator.  And $-2 must be some declmods
	   or declspecs.  We can't move the maybeasm into this rule because
	   we need that reduce so we prefer fn.def1 when appropriate.  */
initdcl0_innards:
	  maybe_attribute '='
		{ $<ttype>$ = parse_decl0 ($<ttype>-1, $<ftype>-2.t,
					   $<ftype>-2.lookups, $1, 1); }
          /* Note how the declaration of the variable is in effect
	     while its init is parsed! */ 
	  init
		{ parse_end_decl ($<ttype>3, $4, $<ttype>0); }
	| maybe_attribute
		{ tree d = parse_decl0 ($<ttype>-1, $<ftype>-2.t,
					$<ftype>-2.lookups, $1, 0);
		  parse_end_decl (d, NULL_TREE, $<ttype>0); }
  	;
  
initdcl0:
	  declarator maybeasm initdcl0_innards
                {}
	;

notype_initdcl0:
          notype_declarator maybeasm initdcl0_innards
                {}
        ;
  
nomods_initdcl0:
          notype_declarator maybeasm
            { /* Set things up as initdcl0_innards expects.  */
	      $<ttype>$ = $2;
	      $2 = $1; 
              $<ftype>1.t = NULL_TREE;
	      $<ftype>1.lookups = NULL_TREE; }
          initdcl0_innards 
            {}
	| constructor_declarator maybeasm maybe_attribute
		{ tree d = parse_decl0 ($1, NULL_TREE, NULL_TREE, $3, 0);
		  parse_end_decl (d, NULL_TREE, $2); }
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
        ;

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
		{ $$ = tree_cons (NULL_TREE, $3, $$); }
	/* These are for labeled elements.  */
	| '[' expr_no_commas ']' init
		{ $$ = build_tree_list ($2, $4); }
	| identifier ':' init
		{ $$ = build_tree_list ($$, $3); }
	| initlist ',' identifier ':' init
		{ $$ = tree_cons ($3, $5, $$); }
	;

pending_inline:
	  PRE_PARSED_FUNCTION_DECL maybe_return_init function_body
		{
		  expand_body (finish_function (2));
		  process_next_inline ($1);
		}
	| PRE_PARSED_FUNCTION_DECL maybe_return_init function_try_block
		{ 
		  expand_body (finish_function (2)); 
                  process_next_inline ($1);
		}
	| PRE_PARSED_FUNCTION_DECL maybe_return_init error
		{ 
		  finish_function (2); 
		  process_next_inline ($1); }
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
        ;

pending_defargs:
	  /* empty */ %prec EMPTY
	| pending_defargs defarg_again
		{ do_pending_defargs (); }
	| pending_defargs error
		{ do_pending_defargs (); }
	;

structsp:
	  ENUM identifier '{'
		{ $<ttype>$ = current_enum_type;
		  current_enum_type = start_enum ($2); }
	  enumlist_opt '}'
		{ $$.t = current_enum_type;
		  finish_enum (current_enum_type);
		  $$.new_type_flag = 1;
		  current_enum_type = $<ttype>4;
		  check_for_missing_semicolon ($$.t); }
	| ENUM '{'
		{ $<ttype>$ = current_enum_type;
		  current_enum_type = start_enum (make_anon_name ()); }
	  enumlist_opt '}'
                { $$.t = current_enum_type;
		  finish_enum (current_enum_type);
		  $$.new_type_flag = 1;
		  current_enum_type = $<ttype>3;
		  check_for_missing_semicolon ($$.t); }
	| ENUM identifier
		{ $$.t = xref_tag (enum_type_node, $2, 1); 
		  $$.new_type_flag = 0; }
	| ENUM complex_type_name
		{ $$.t = xref_tag (enum_type_node, $2, 1); 
		  $$.new_type_flag = 0; }
	| TYPENAME_KEYWORD typename_sub
		{ $$.t = $2;
		  $$.new_type_flag = 0; 
		  if (!processing_template_decl)
		    pedwarn ("using `typename' outside of template"); }
	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head_defn maybe_base_class_list '{'
		{
		  if ($2 && $1.t != error_mark_node)
		    {
		      tree type = TREE_TYPE ($1.t);
		  
		      if (TREE_CODE (type) == TYPENAME_TYPE)
			/* In a definition of a member class template,
                           we will get here with an implicit typename,
                           a TYPENAME_TYPE with a type. */
			type = TREE_TYPE (type);
		      maybe_process_partial_specialization (type);
		      xref_basetypes (current_aggr, $1.t, type, $2);
		    }
		  $1.t = begin_class_definition (TREE_TYPE ($1.t)); 
		  check_class_key (current_aggr, $1.t);
                  current_aggr = NULL_TREE; }
          opt.component_decl_list '}' maybe_attribute
		{ 
		  int semi;
		  tree t;

		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  semi = yychar == ';';

		  t = finish_class_definition ($1.t, $7, semi, $1.new_type_flag);
		  $<ttype>$ = t;

		  /* restore current_aggr */
		  current_aggr = TREE_CODE (t) != RECORD_TYPE
				 ? union_type_node
				 : CLASSTYPE_DECLARED_CLASS (t)
				 ? class_type_node : record_type_node;
		}
	  pending_defargs
                {
		  done_pending_defargs ();
		  begin_inline_definitions ();
		}
	  pending_inlines
                {
		  finish_inline_definitions ();
		  $$.t = $<ttype>8;
		  $$.new_type_flag = 1; 
		}
	| class_head_decl
		{
		  $$.t = TREE_TYPE ($1.t);
		  $$.new_type_flag = $1.new_type_flag;
		  check_class_key (current_aggr, $$.t);
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
	| aggr attributes
		{ $$ = build_tree_list ($2, $1); }
	;

class_head:
	  aggr identifier
		{
		  current_aggr = $1;
		  $$ = build_tree_list (NULL_TREE, $2);
		}
	| aggr nested_name_specifier identifier
		{
		  current_aggr = $1;
		  $$ = build_tree_list ($2, $3);
		}
	| aggr global_scope nested_name_specifier identifier
		{
		  current_aggr = $1;
		  $$ = build_tree_list ($3, $4);
		}
	| aggr global_scope identifier
		{
		  current_aggr = $1;
		  $$ = build_tree_list (global_namespace, $3);
		}
	;

class_head_apparent_template:
	  aggr apparent_template_type
		{ 
		  current_aggr = $1; 
		  $$ = $2;
		}
	| aggr nested_name_specifier apparent_template_type
		{ 
		  current_aggr = $1; 
		  $$ = $3;
		}
	| aggr global_scope nested_name_specifier apparent_template_type
		{ 
		  current_aggr = $1; 
		  $$ = $4;
		}
	;

class_head_decl:
	  class_head %prec EMPTY
		{
		  $$.t = handle_class_head (current_aggr,
					    TREE_PURPOSE ($1), TREE_VALUE ($1),
					    0, &$$.new_type_flag);
		}
	| aggr identifier_defn %prec EMPTY
		{
		  current_aggr = $1;
		  $$.t = TYPE_MAIN_DECL (xref_tag (current_aggr, $2, 0));
		  $$.new_type_flag = 1;
		}
	| class_head_apparent_template %prec EMPTY
		{
		  $$.t = $1;
		  $$.new_type_flag = 0;
		}
	;

class_head_defn:
	  class_head '{'
		{
		  yyungetc ('{', 1);
		  $$.t = handle_class_head (current_aggr,
					    TREE_PURPOSE ($1), TREE_VALUE ($1),
					    1, &$$.new_type_flag);
		}
	| class_head ':'
		{
		  yyungetc (':', 1);
		  $$.t = handle_class_head (current_aggr,
					    TREE_PURPOSE ($1), TREE_VALUE ($1),
					    1, &$$.new_type_flag);
		}
	| class_head_apparent_template '{'
		{
		  yyungetc ('{', 1);
		  $$.t = $1;
		  $$.new_type_flag = 0;
		  if (TREE_CODE (TREE_TYPE ($1)) == RECORD_TYPE)
		    /* We might be specializing a template with a different
		       class-key.  */
		    CLASSTYPE_DECLARED_CLASS (TREE_TYPE ($1))
		      = (current_aggr == class_type_node);
		}
	| class_head_apparent_template ':'
		{
		  yyungetc (':', 1);
		  $$.t = $1;
		  $$.new_type_flag = 0;
		  if (TREE_CODE (TREE_TYPE ($1)) == RECORD_TYPE)
		    /* We might be specializing a template with a different
		       class-key.  */
		    CLASSTYPE_DECLARED_CLASS (TREE_TYPE ($1))
		      = (current_aggr == class_type_node);
		}
	| aggr identifier_defn '{'
		{
		  yyungetc ('{', 1);
		  current_aggr = $1;
		  $$.t = handle_class_head (current_aggr,
					    NULL_TREE, $2,
					    1, &$$.new_type_flag);
		}
	| aggr identifier_defn ':'
		{
		  yyungetc (':', 1);
		  current_aggr = $1;
		  $$.t = handle_class_head (current_aggr,
					    NULL_TREE, $2,
					    1, &$$.new_type_flag);
		}
        | aggr '{'
		{
		  current_aggr = $1;
		  $$.t = TYPE_MAIN_DECL (xref_tag ($1, make_anon_name (), 0));
		  $$.new_type_flag = 0;
		  yyungetc ('{', 1);
		}
	;

maybe_base_class_list:
	  /* empty */
		{ $$ = NULL_TREE; }
	| ':' see_typename
		{ error ("no bases given following `:'");
		  $$ = NULL_TREE; }
	| ':' see_typename base_class_list
		{ $$ = $3; }
	;

base_class_list:
	  base_class
	| base_class_list ',' see_typename base_class
		{ $$ = chainon ($$, $4); }
	;

base_class:
	  base_class.1
		{ $$ = finish_base_specifier (access_default_node, $1); }
	| base_class_access_list see_typename base_class.1
                { $$ = finish_base_specifier ($1, $3); }
	;

base_class.1:
	  typename_sub
		{ if (!TYPE_P ($$))
		    $$ = error_mark_node; }
	| nonnested_type
		{ $$ = TREE_TYPE ($$); }
	;

base_class_access_list:
	  VISSPEC see_typename
	| SCSPEC see_typename
		{ if ($1 != ridpointers[(int)RID_VIRTUAL])
		    error ("`%D' access", $1);
		  $$ = access_default_virtual_node; }
	| base_class_access_list VISSPEC see_typename
		{
		  if ($1 != access_default_virtual_node)
		    error ("multiple access specifiers");
		  else if ($2 == access_public_node)
		    $$ = access_public_virtual_node;
		  else if ($2 == access_protected_node)
		    $$ = access_protected_virtual_node;
		  else /* $2 == access_private_node */
		    $$ = access_private_virtual_node;
		}
	| base_class_access_list SCSPEC see_typename
		{ if ($2 != ridpointers[(int)RID_VIRTUAL])
		    error ("`%D' access", $2);
		  else if ($$ == access_public_node)
		    $$ = access_public_virtual_node;
		  else if ($$ == access_protected_node)
		    $$ = access_protected_virtual_node;
		  else if ($$ == access_private_node)
		    $$ = access_private_virtual_node;
		  else
		    error ("multiple `virtual' specifiers");
		}
	;

opt.component_decl_list:
	| component_decl_list
	| opt.component_decl_list access_specifier component_decl_list
	| opt.component_decl_list access_specifier 
	;

access_specifier:
	  VISSPEC ':'
                {
		  current_access_specifier = $1;
                }
	;

/* Note: we no longer warn about the semicolon after a component_decl_list.
   ARM $9.2 says that the semicolon is optional, and therefore allowed.  */
component_decl_list:
	  component_decl
		{ 
		  finish_member_declaration ($1);
		  current_aggr = NULL_TREE;
		  reset_type_access_control ();
		}
	| component_decl_list component_decl
		{ 
		  finish_member_declaration ($2);
		  current_aggr = NULL_TREE;
		  reset_type_access_control ();
		}
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
	| fn.def2 TRY /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 RETURN_KEYWORD /* base_init compstmt */
		{ $$ = finish_method ($$); }
	| fn.def2 '{' /* nodecls compstmt */
		{ $$ = finish_method ($$); }
	| ';'
		{ $$ = NULL_TREE; }
	| extension component_decl
		{ $$ = $2;
		  pedantic = $1; }
        | template_header component_decl
                {  
		  if ($2)
		    $$ = finish_member_template_decl ($2);
		  else
		    /* The component was already processed.  */
		    $$ = NULL_TREE;

		  finish_template_decl ($1);
		}
	| template_header typed_declspecs ';'
                { 
		  $$ = finish_member_class_template ($2.t); 
		  finish_template_decl ($1);
		}
	| bad_decl
		{ $$ = NULL_TREE; }
	;

component_decl_1:
	/* Do not add a "typed_declspecs declarator" rule here for
	   speed; we need to call grok_x_components for enums, so the
	   speedup would be insignificant.  */
	  typed_declspecs components
		{
		  /* Most of the productions for component_decl only
		     allow the creation of one new member, so we call
		     finish_member_declaration in component_decl_list.
		     For this rule and the next, however, there can be
		     more than one member, e.g.:

		       int i, j;

		     and we need the first member to be fully
		     registered before the second is processed.
		     Therefore, the rules for components take care of
		     this processing.  To avoid registering the
		     components more than once, we send NULL_TREE up
		     here; that lets finish_member_declaration know
		     that there is nothing to do.  */
		  if (!$2)
		    grok_x_components ($1.t);
		  $$ = NULL_TREE;
		}
	| declmods notype_components
		{ 
		  if (!$2)
		    grok_x_components ($1.t);
		  $$ = NULL_TREE; 
		}
	| notype_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2, $3); }
	| constructor_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2, $3); }
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
		  split_specs_attrs ($1.t, &specs, &attrs);
		  $$ = grokfield ($2, specs, $5, $3,
				  chainon ($4, attrs)); }
	| component_constructor_declarator maybeasm maybe_attribute maybe_init
		{ $$ = grokfield ($$, NULL_TREE, $4, $2, $3); }
	| using_decl
		{ $$ = do_class_using_decl ($1); }
        ;

/* The case of exactly one component is handled directly by component_decl.  */
/* ??? Huh? ^^^ */
components:
	  /* empty: possibly anonymous */
                { $$ = 0; }
	| component_declarator0
                { 
		  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
		    $1 = finish_member_template_decl ($1);
		  finish_member_declaration ($1); 
		  $$ = 1;
		}
	| components ',' component_declarator
                { 
		  check_multiple_declarators ();
		  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
		    $3 = finish_member_template_decl ($3);
		  finish_member_declaration ($3);
		  $$ = 2;
		}
	;

notype_components:
	  /* empty: possibly anonymous */
                { $$ = 0; }
	| notype_component_declarator0
                { 
		  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
		    $1 = finish_member_template_decl ($1);
		  finish_member_declaration ($1);
		  $$ = 1;
		}
	| notype_components ',' notype_component_declarator
                { 
		  check_multiple_declarators ();
		  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
		    $3 = finish_member_template_decl ($3);
		  finish_member_declaration ($3); 
		  $$ = 2;
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
		{ $$ = parse_field0 ($1, $<ftype>0.t, $<ftype>0.lookups,
				     $3, $2, $4); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield0 ($1, $<ftype>0.t, $<ftype>0.lookups,
					$4, $3); }
	;

notype_component_declarator0:
	  notype_declarator maybeasm maybe_attribute maybe_init
		{ $$ = parse_field0 ($1, $<ftype>0.t, $<ftype>0.lookups,
				     $3, $2, $4); }
	| constructor_declarator maybeasm maybe_attribute maybe_init
		{ $$ = parse_field0 ($1, $<ftype>0.t, $<ftype>0.lookups,
				     $3, $2, $4); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield0 ($1, $<ftype>0.t, $<ftype>0.lookups,
					$4, $3); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield0 (NULL_TREE, $<ftype>0.t,
					$<ftype>0.lookups, $3, $2); }
	;

after_type_component_declarator:
	  after_type_declarator maybeasm maybe_attribute maybe_init
		{ $$ = parse_field ($1, $3, $2, $4); }
	| TYPENAME ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield ($1, $4, $3); }
	;

notype_component_declarator:
	  notype_declarator maybeasm maybe_attribute maybe_init
		{ $$ = parse_field ($1, $3, $2, $4); }
	| IDENTIFIER ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield ($1, $4, $3); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = parse_bitfield (NULL_TREE, $3, $2); }
	;

enumlist_opt:
	  enumlist maybecomma_warn
	| maybecomma_warn
	;

/* We chain the enumerators in reverse order.
   Because of the way enums are built, the order is
   insignificant.  Take advantage of this fact.  */

enumlist:
	  enumerator
	| enumlist ',' enumerator
	;

enumerator:
	  identifier
		{ build_enumerator ($1, NULL_TREE, current_enum_type); }
	| identifier '=' expr_no_commas
		{ build_enumerator ($1, $3, current_enum_type); }
	;

/* ISO new-type-id (5.3.4) */
new_type_id:
	  type_specifier_seq new_declarator
		{ $$.t = build_tree_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| type_specifier_seq  %prec EMPTY
		{ $$.t = build_tree_list ($1.t, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	/* GNU extension to allow arrays of arbitrary types with
	   non-constant dimension.  */
	| '(' type_id ')' '[' expr ']'
		{
		  if (pedantic)
		    pedwarn ("ISO C++ forbids array dimensions with parenthesized type in new");
		  $$.t = build_nt (ARRAY_REF, TREE_VALUE ($2.t), $5);
		  $$.t = build_tree_list (TREE_PURPOSE ($2.t), $$.t);
		  $$.new_type_flag = $2.new_type_flag;
		}
	;

cv_qualifiers:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| cv_qualifiers CV_QUALIFIER
		{ $$ = tree_cons (NULL_TREE, $2, $$); }
	;

nonempty_cv_qualifiers:
	  CV_QUALIFIER
		{ $$.t = hash_tree_cons (NULL_TREE, $1, NULL_TREE);
		  $$.new_type_flag = 0; }
	| nonempty_cv_qualifiers CV_QUALIFIER
		{ $$.t = hash_tree_cons (NULL_TREE, $2, $1.t); 
		  $$.new_type_flag = $1.new_type_flag; }
	| attributes %prec EMPTY
		{ $$.t = hash_tree_cons ($1, NULL_TREE, NULL_TREE); 
		  $$.new_type_flag = 0; }
	| nonempty_cv_qualifiers attributes %prec EMPTY
		{ $$.t = hash_tree_cons ($2, NULL_TREE, $1.t); 
		  $$.new_type_flag = $1.new_type_flag; }
	;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are preferred.  */

/* An expression which will not live on the momentary obstack.  */
maybe_parmlist:
	  '(' nonnull_exprlist ')'
		{ $$ = $2; }
	| '(' parmlist ')'
		{ $$ = $2; }
	| LEFT_RIGHT
		{ $$ = empty_parms (); }
	| '(' error ')'
		{ $$ = NULL_TREE; }
	;

/* A declarator that is allowed only after an explicit typespec.  */

after_type_declarator_intern:
	  after_type_declarator
	| attributes after_type_declarator
                {
		  /* Provide support for '(' attributes '*' declarator ')'
		     etc */
		  $$ = tree_cons ($1, $2, NULL_TREE);
		}
	;

/* may all be followed by prec '.' */
after_type_declarator:
	  '*' nonempty_cv_qualifiers after_type_declarator_intern  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers after_type_declarator_intern  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' after_type_declarator_intern  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' after_type_declarator_intern  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers after_type_declarator_intern
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| direct_after_type_declarator
	;

direct_after_type_declarator:
	  direct_after_type_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $2, $3, $4); }
	| direct_after_type_declarator '[' expr ']'
		{ $$ = build_nt (ARRAY_REF, $$, $3); }
	| direct_after_type_declarator '[' ']'
		{ $$ = build_nt (ARRAY_REF, $$, NULL_TREE); }
	| '(' after_type_declarator_intern ')'
		{ $$ = $2; }
	| nested_name_specifier type_name  %prec EMPTY
		{ push_nested_class ($1, 3);
		  $$ = build_nt (SCOPE_REF, $$, $2);
		  TREE_COMPLEXITY ($$) = current_class_depth; }
	| type_name  %prec EMPTY
	;

nonnested_type:
	  type_name  %prec EMPTY
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    {
		      $$ = lookup_name ($1, 1);
		      maybe_note_name_used_in_class ($1, $$);
		    }
		  else
		    $$ = $1;
		}
	| global_scope type_name
		{
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    $$ = IDENTIFIER_GLOBAL_VALUE ($2);
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

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator_intern:
	  notype_declarator
	| attributes notype_declarator
                {
		  /* Provide support for '(' attributes '*' declarator ')'
		     etc */
		  $$ = tree_cons ($1, $2, NULL_TREE);
		}
	;
	
notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator_intern  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers notype_declarator_intern  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' notype_declarator_intern  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' notype_declarator_intern  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers notype_declarator_intern
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| direct_notype_declarator
	;

complex_notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator_intern  %prec UNARY
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '&' nonempty_cv_qualifiers notype_declarator_intern  %prec UNARY
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '*' complex_notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '&' complex_notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| ptr_to_mem cv_qualifiers notype_declarator_intern
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| complex_direct_notype_declarator
	;

complex_direct_notype_declarator:
	  direct_notype_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $2, $3, $4); }
	| '(' complex_notype_declarator ')'
		{ $$ = $2; }
	| direct_notype_declarator '[' expr ']'
		{ $$ = build_nt (ARRAY_REF, $$, $3); }
	| direct_notype_declarator '[' ']'
		{ $$ = build_nt (ARRAY_REF, $$, NULL_TREE); }
	| notype_qualified_id
                { enter_scope_of ($1); }
	| global_scope notype_qualified_id
                { enter_scope_of ($2); $$ = $2;}
	| global_scope notype_unqualified_id
                { $$ = build_nt (SCOPE_REF, global_namespace, $2);
		  enter_scope_of ($$); 
		}
        | nested_name_specifier notype_template_declarator
                { got_scope = NULL_TREE;
		  $$ = build_nt (SCOPE_REF, $1, $2);
		  enter_scope_of ($$);
		}
	;

qualified_id:
	  nested_name_specifier unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_nt (SCOPE_REF, $$, $2); }
        | nested_name_specifier object_template_id
                { got_scope = NULL_TREE;
 		  $$ = build_nt (SCOPE_REF, $1, $2); }
	;

notype_qualified_id:
	  nested_name_specifier notype_unqualified_id
		{ got_scope = NULL_TREE;
		  $$ = build_nt (SCOPE_REF, $$, $2); }
        | nested_name_specifier object_template_id
                { got_scope = NULL_TREE;
		  $$ = build_nt (SCOPE_REF, $1, $2); }
	;

overqualified_id:
	  notype_qualified_id
	| global_scope notype_qualified_id
		{ $$ = $2; }
	;

functional_cast:
	  typespec '(' nonnull_exprlist ')'
		{ $$ = build_functional_cast ($1.t, $3); }
	| typespec '(' expr_or_declarator_intern ')'
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
	| nested_name_specifier TEMPLATE explicit_template_type SCOPE
                { got_scope = $$ 
		    = make_typename_type ($1, $3, tf_error); }
	/* Error handling per Core 125.  */
	| nested_name_specifier IDENTIFIER SCOPE
                { got_scope = $$ 
		    = make_typename_type ($1, $2, tf_error); }
	| nested_name_specifier PTYPENAME SCOPE
                { got_scope = $$ 
		    = make_typename_type ($1, $2, tf_error); }
	;

/* Why the @#$%^& do type_name and notype_identifier need to be expanded
   inline here?!?  (jason) */
nested_name_specifier_1:
	  TYPENAME SCOPE
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    {
		      $$ = lastiddecl;
		      maybe_note_name_used_in_class ($1, $$);
		    }
		  got_scope = $$ =
		    complete_type (TYPE_MAIN_VARIANT (TREE_TYPE ($$)));
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
	;

typename_sub:
	  typename_sub0
	| global_scope typename_sub0
		{ $$ = $2; }
	;

typename_sub0:
	  typename_sub1 identifier %prec EMPTY
		{
		  if (TYPE_P ($1))
		    $$ = make_typename_type ($1, $2, tf_error);
		  else if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    error ("`%T' is not a class or namespace", $2);
		  else
		    {
		      $$ = $2;
		      if (TREE_CODE ($$) == TYPE_DECL)
			$$ = TREE_TYPE ($$);
		    }
		}
	| typename_sub1 template_type %prec EMPTY
		{ $$ = TREE_TYPE ($2); }
	| typename_sub1 explicit_template_type %prec EMPTY
                { $$ = make_typename_type ($1, $2, tf_error); }
	| typename_sub1 TEMPLATE explicit_template_type %prec EMPTY
                { $$ = make_typename_type ($1, $3, tf_error); }
	;

typename_sub1:
	  typename_sub2
		{
		  if (TREE_CODE ($1) == IDENTIFIER_NODE)
		    error ("`%T' is not a class or namespace", $1);
		  else if (TREE_CODE ($1) == TYPE_DECL)
		    $$ = TREE_TYPE ($1);
		}
	| typename_sub1 typename_sub2
		{
		  if (TYPE_P ($1))
		    $$ = make_typename_type ($1, $2, tf_error);
		  else if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    error ("`%T' is not a class or namespace", $2);
		  else
		    {
		      $$ = $2;
		      if (TREE_CODE ($$) == TYPE_DECL)
			$$ = TREE_TYPE ($$);
		    }
		}
	| typename_sub1 explicit_template_type SCOPE
                { got_scope = $$ 
		    = make_typename_type ($1, $2, tf_error); }
	| typename_sub1 TEMPLATE explicit_template_type SCOPE
                { got_scope = $$ 
		    = make_typename_type ($1, $3, tf_error); }
	;

/* This needs to return a TYPE_DECL for simple names so that we don't
   forget what name was used.  */
typename_sub2:
	  TYPENAME SCOPE
		{
		  if (TREE_CODE ($1) != TYPE_DECL)
		    $$ = lastiddecl;

		  /* Retrieve the type for the identifier, which might involve
		     some computation. */
		  got_scope = complete_type (TREE_TYPE ($$));

		  if ($$ == error_mark_node)
		    error ("`%T' is not a class or namespace", $1);
		}
	| SELFNAME SCOPE
		{
		  if (TREE_CODE ($1) != TYPE_DECL)
		    $$ = lastiddecl;
		  got_scope = complete_type (TREE_TYPE ($$));
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

explicit_template_type:
	  identifier '<' template_arg_list_opt template_close_bracket
		{ $$ = build_min_nt (TEMPLATE_ID_EXPR, $1, $3); }
	;

complex_type_name:
	  global_scope type_name
		{
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    $$ = IDENTIFIER_GLOBAL_VALUE ($2);
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

/* ISO new-declarator (5.3.4) */
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
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem cv_qualifiers new_declarator
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| direct_new_declarator  %prec EMPTY
	;

/* ISO direct-new-declarator (5.3.4) */
direct_new_declarator:
	  '[' expr ']'
		{ $$ = build_nt (ARRAY_REF, NULL_TREE, $2); }
	| direct_new_declarator '[' expr ']'
		{ $$ = build_nt (ARRAY_REF, $$, $3); }
	;

absdcl_intern:
	  absdcl
	| attributes absdcl
                {
		  /* Provide support for '(' attributes '*' declarator ')'
		     etc */
		  $$ = tree_cons ($1, $2, NULL_TREE);
		}
	;
	
/* ISO abstract-declarator (8.1) */
absdcl:
	  '*' nonempty_cv_qualifiers absdcl_intern
		{ $$ = make_pointer_declarator ($2.t, $3); }
	| '*' absdcl_intern
		{ $$ = make_pointer_declarator (NULL_TREE, $2); }
	| '*' nonempty_cv_qualifiers  %prec EMPTY
		{ $$ = make_pointer_declarator ($2.t, NULL_TREE); }
	| '*'  %prec EMPTY
		{ $$ = make_pointer_declarator (NULL_TREE, NULL_TREE); }
	| '&' nonempty_cv_qualifiers absdcl_intern
		{ $$ = make_reference_declarator ($2.t, $3); }
	| '&' absdcl_intern
		{ $$ = make_reference_declarator (NULL_TREE, $2); }
	| '&' nonempty_cv_qualifiers  %prec EMPTY
		{ $$ = make_reference_declarator ($2.t, NULL_TREE); }
	| '&'  %prec EMPTY
		{ $$ = make_reference_declarator (NULL_TREE, NULL_TREE); }
	| ptr_to_mem cv_qualifiers  %prec EMPTY
		{ tree arg = make_pointer_declarator ($2, NULL_TREE);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| ptr_to_mem cv_qualifiers absdcl_intern
		{ tree arg = make_pointer_declarator ($2, $3);
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	| direct_abstract_declarator  %prec EMPTY
	;

/* ISO direct-abstract-declarator (8.1) */
direct_abstract_declarator:
	  '(' absdcl_intern ')'
		{ $$ = $2; }
	  /* `(typedef)1' is `int'.  */
	| direct_abstract_declarator '(' parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, $3, $5, $6); }
	| direct_abstract_declarator LEFT_RIGHT cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator ($$, empty_parms (), $3, $4); }
	| direct_abstract_declarator '[' expr ']'  %prec '.'
		{ $$ = build_nt (ARRAY_REF, $$, $3); }
	| direct_abstract_declarator '[' ']'  %prec '.'
		{ $$ = build_nt (ARRAY_REF, $$, NULL_TREE); }
	| '(' complex_parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
		{ $$ = make_call_declarator (NULL_TREE, $2, $4, $5); }
	| regcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
		{ set_quals_and_spec ($$, $2, $3); }
	| fcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
		{ set_quals_and_spec ($$, $2, $3); }
	| '[' expr ']'  %prec '.'
		{ $$ = build_nt (ARRAY_REF, NULL_TREE, $2); }
	| '[' ']'  %prec '.'
		{ $$ = build_nt (ARRAY_REF, NULL_TREE, NULL_TREE); }
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

/* Read zero or more forward-declarations for labels
   that nested functions can jump to.  */
maybe_label_decls:
	  /* empty */
	| label_decls
		{ if (pedantic)
		    pedwarn ("ISO C++ forbids label declarations"); }
	;

label_decls:
	  label_decl
	| label_decls label_decl
	;

label_decl:
	  LABEL identifiers_or_typenames ';'
                { 
		  while ($2)
		    {
		      finish_label_decl (TREE_VALUE ($2));
		      $2 = TREE_CHAIN ($2);
		    }
		}
	;

compstmt_or_stmtexpr:
	  save_lineno '{'
                { $<ttype>$ = begin_compound_stmt (0); }
	  compstmtend 
                { STMT_LINENO ($<ttype>3) = $1;
		  finish_compound_stmt (0, $<ttype>3); }
	;

compstmt:
	  compstmt_or_stmtexpr
		{ last_expr_type = NULL_TREE; }
	;

simple_if:
	  IF
		{ $<ttype>$ = begin_if_stmt ();
		  cond_stmt_keyword = "if"; }
            paren_cond_or_null
                { finish_if_stmt_cond ($3, $<ttype>2); }
	    implicitly_scoped_stmt
                { $$ = $<ttype>2;
		  finish_then_clause ($<ttype>2); }
	;

implicitly_scoped_stmt:
	  compstmt
	| 
		{ $<ttype>$ = begin_compound_stmt (0); }
	  save_lineno simple_stmt 
		{ STMT_LINENO ($<ttype>1) = $2;
		  if ($3) STMT_LINENO ($3) = $2;
		  finish_compound_stmt (0, $<ttype>1); }
	;

stmt:
	  compstmt
	| save_lineno simple_stmt
		{ if ($2) STMT_LINENO ($2) = $1; }
	;

simple_stmt:
	  decl
		{ finish_stmt ();
		  $$ = NULL_TREE; }
	| expr ';'
                { $$ = finish_expr_stmt ($1); }
	| simple_if ELSE
                { begin_else_clause (); }
	  implicitly_scoped_stmt
                { 
		  $$ = $1;
		  finish_else_clause ($1); 
		  finish_if_stmt ();
		}
	| simple_if  %prec IF
                { $$ = $1;
		  finish_if_stmt (); }
	| WHILE
		{
		  $<ttype>$ = begin_while_stmt ();
		  cond_stmt_keyword = "while";
		}
	  paren_cond_or_null
                { finish_while_stmt_cond ($3, $<ttype>2); }
	  implicitly_scoped_stmt
                { $$ = $<ttype>2;
		  finish_while_stmt ($<ttype>2); }
	| DO
                { $<ttype>$ = begin_do_stmt (); }
	  implicitly_scoped_stmt WHILE
		{
		  finish_do_body ($<ttype>2);
		  cond_stmt_keyword = "do";
		}
	  paren_expr_or_null ';'
                { $$ = $<ttype>2;
		  finish_do_stmt ($6, $<ttype>2); }
	| FOR
                { $<ttype>$ = begin_for_stmt (); }
	  '(' for.init.statement
                { finish_for_init_stmt ($<ttype>2); }
	  xcond ';'
                { finish_for_cond ($6, $<ttype>2); }
	  xexpr ')'
                { finish_for_expr ($9, $<ttype>2); }
	  implicitly_scoped_stmt
                { $$ = $<ttype>2;
		  finish_for_stmt ($<ttype>2); }
	| SWITCH 
                { $<ttype>$ = begin_switch_stmt (); }
	    '(' condition ')'
                { finish_switch_cond ($4, $<ttype>2); }
	  implicitly_scoped_stmt
                { $$ = $<ttype>2;
		  finish_switch_stmt ($<ttype>2); }
	| CASE expr_no_commas ':'
                { $<ttype>$ = finish_case_label ($2, NULL_TREE); }
	  stmt
		{ $$ = $<ttype>4; }
	| CASE expr_no_commas ELLIPSIS expr_no_commas ':'
                { $<ttype>$ = finish_case_label ($2, $4); }
	  stmt
		{ $$ = $<ttype>6; }
	| DEFAULT ':'
		{ $<ttype>$ = finish_case_label (NULL_TREE, NULL_TREE); }
	  stmt
		{ $$ = $<ttype>3; }
	| BREAK ';'
                { $$ = finish_break_stmt (); }
	| CONTINUE ';'
                { $$ = finish_continue_stmt (); }
	| RETURN_KEYWORD ';'
                { $$ = finish_return_stmt (NULL_TREE); }
	| RETURN_KEYWORD expr ';'
                { $$ = finish_return_stmt ($2); }
	| asm_keyword maybe_cv_qualifier '(' string ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, NULL_TREE, NULL_TREE,
					NULL_TREE);
		  ASM_INPUT_P ($$) = 1; }
	/* This is the case with just output operands.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, $6, NULL_TREE, NULL_TREE); }
	/* This is the case with input operands as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':'
	  asm_operands ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, $6, $8, NULL_TREE); }
	| asm_keyword maybe_cv_qualifier '(' string SCOPE asm_operands ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, NULL_TREE, $6, NULL_TREE); }
	/* This is the case with clobbered registers as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':'
	  asm_operands ':' asm_clobbers ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, $6, $8, $10); }
	| asm_keyword maybe_cv_qualifier '(' string SCOPE asm_operands ':'
	  asm_clobbers ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, NULL_TREE, $6, $8); }
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands SCOPE
	  asm_clobbers ')' ';'
		{ $$ = finish_asm_stmt ($2, $4, $6, NULL_TREE, $8); }
	| GOTO '*' expr ';'
                { 
		  if (pedantic)
		    pedwarn ("ISO C++ forbids computed gotos");
		  $$ = finish_goto_stmt ($3);
		}
	| GOTO identifier ';'
                { $$ = finish_goto_stmt ($2); }
	| label_colon stmt
		{ $$ = NULL_TREE; }
	| label_colon '}'
		{ error ("label must be followed by statement");
		  yyungetc ('}', 0);
		  $$ = NULL_TREE; }
	| ';'
		{ finish_stmt ();
		  $$ = NULL_TREE; }
	| try_block
		{ $$ = NULL_TREE; }
	| using_directive
		{ $$ = NULL_TREE; }
	| namespace_using_decl
	        { do_local_using_decl ($1);
		  $$ = NULL_TREE; }
	| namespace_alias
		{ $$ = NULL_TREE; }
	;

function_try_block:
	  TRY
		{ $<ttype>$ = begin_function_try_block (); }
	  function_body
		{ finish_function_try_block ($<ttype>2); }
	  handler_seq
		{ finish_function_handler_sequence ($<ttype>2); }
	;

try_block:
	  TRY
                { $<ttype>$ = begin_try_block (); }
	  compstmt
                { finish_try_block ($<ttype>2); }
	  handler_seq
                { finish_handler_sequence ($<ttype>2); }
	;

handler_seq:
	  handler
	| handler_seq handler
	| /* empty */
		{ /* Generate a fake handler block to avoid later aborts. */
		  tree fake_handler = begin_handler ();
		  finish_handler_parms (NULL_TREE, fake_handler);
		  finish_handler (fake_handler);
		  $<ttype>$ = fake_handler;

		  error ("must have at least one catch per try block");
		}
	;

handler:
	  CATCH
                { $<ttype>$ = begin_handler (); }
          handler_args
                { finish_handler_parms ($3, $<ttype>2); }
	  compstmt
                { finish_handler ($<ttype>2); }
	;

type_specifier_seq:
	  typed_typespecs  %prec EMPTY
	| nonempty_cv_qualifiers  %prec EMPTY
	;

handler_args:
	  '(' ELLIPSIS ')'
		{ $$ = NULL_TREE; }
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
		{ 
		  check_for_new_type ("inside exception declarations", $2);
		  $$ = start_handler_parms (TREE_PURPOSE ($2.t),
					    TREE_VALUE ($2.t));
		}
	;

label_colon:
	  IDENTIFIER ':'
                { finish_label_stmt ($1); }
	| PTYPENAME ':'
                { finish_label_stmt ($1); }
	| TYPENAME ':'
                { finish_label_stmt ($1); }
	| SELFNAME ':'
                { finish_label_stmt ($1); }
	;

for.init.statement:
	  xexpr ';'
                { finish_expr_stmt ($1); }
	| decl
	| '{' compstmtend
		{ if (pedantic)
		    pedwarn ("ISO C++ forbids compound statements inside for initializations");
		}
	;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_cv_qualifier:
	  /* empty */
		{ $$ = NULL_TREE; }
	| CV_QUALIFIER
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
		{ $$ = build_tree_list (build_tree_list (NULL_TREE, $1), $3); }
	| '[' identifier ']' STRING '(' expr ')'
		{ $$ = build_tree_list (build_tree_list ($2, $4), $6); }
	;

asm_clobbers:
	  string
		{ $$ = tree_cons (NULL_TREE, combine_strings ($1), NULL_TREE);}
	| asm_clobbers ',' string
		{ $$ = tree_cons (NULL_TREE, combine_strings ($3), $1); }
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
		{ $$ = finish_parmlist (build_tree_list (NULL_TREE, $1.t), 0);
		  check_for_new_type ("inside parameter list", $1); }
	;

/* This nonterminal does not include the common sequence '(' type_id ')',
   as it is ambiguous and must be disambiguated elsewhere.  */
complex_parmlist:
	  parms
                { $$ = finish_parmlist ($$, 0); }
	| parms_comma ELLIPSIS
                { $$ = finish_parmlist ($1, 1); }
	/* C++ allows an ellipsis without a separating ',' */
	| parms ELLIPSIS
                { $$ = finish_parmlist ($1, 1); }
	| type_id ELLIPSIS
                { $$ = finish_parmlist (build_tree_list (NULL_TREE,
							 $1.t), 1); } 
	| ELLIPSIS
                { $$ = finish_parmlist (NULL_TREE, 1); }
	| parms ':'
		{
		  /* This helps us recover from really nasty
		     parse errors, for example, a missing right
		     parenthesis.  */
		  yyerror ("possibly missing ')'");
		  $$ = finish_parmlist ($1, 0);
		  yyungetc (':', 0);
		  yychar = ')';
		}
	| type_id ':'
		{
		  /* This helps us recover from really nasty
		     parse errors, for example, a missing right
		     parenthesis.  */
		  yyerror ("possibly missing ')'");
		  $$ = finish_parmlist (build_tree_list (NULL_TREE,
							 $1.t), 0); 
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
		{ $$.new_type_flag = $1.new_type_flag;
		  $$.t = build_tree_list ($1.t, $2); }
	| typed_typespecs declarator
		{ $$.t = build_tree_list ($1.t, $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typespec declarator
		{ $$.t = build_tree_list (build_tree_list (NULL_TREE, $1.t),
					  $2); 
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_declspecs1 absdcl
		{ $$.t = build_tree_list ($1.t, $2);
		  $$.new_type_flag = $1.new_type_flag; }
	| typed_declspecs1  %prec EMPTY
		{ $$.t = build_tree_list ($1.t, NULL_TREE); 
		  $$.new_type_flag = $1.new_type_flag; }
	| declmods notype_declarator
		{ $$.t = build_tree_list ($1.t, $2); 
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
		  if (TREE_CODE ($$) == SCOPE_REF)
		    {
		      if (TREE_CODE (TREE_OPERAND ($$, 0)) == TEMPLATE_TYPE_PARM
			  || TREE_CODE (TREE_OPERAND ($$, 0)) == BOUND_TEMPLATE_TEMPLATE_PARM)
			error ("`%E' is not a type, use `typename %E' to make it one", $$);
		      else
			error ("no type `%D' in `%T'", TREE_OPERAND ($$, 1), TREE_OPERAND ($$, 0));
		    }
		  else
		    error ("type specifier omitted for parameter `%E'", $$);
		  $$ = build_tree_list (integer_type_node, $$);
		}
	;

bad_decl:
          IDENTIFIER template_arg_list_ignore IDENTIFIER arg_list_ignore ';'
		{
                  error("'%D' is used as a type, but is not defined as a type.", $1);
                  $3 = error_mark_node;
		}
        ;

template_arg_list_ignore:
          '<' template_arg_list_opt template_close_bracket
		{ }
	| /* empty */
	;

arg_list_ignore:
          '(' nonnull_exprlist ')'
		{ }
	| /* empty */
	;

exception_specification_opt:
	  /* empty */  %prec EMPTY
		{ $$ = NULL_TREE; }
	| THROW '(' ansi_raise_identifiers  ')'  %prec EMPTY
		{ $$ = $3; }
	| THROW LEFT_RIGHT  %prec EMPTY
		{ $$ = empty_except_spec; }
	;

ansi_raise_identifier:
	  type_id
		{
		  check_for_new_type ("exception specifier", $1);
		  $$ = groktypename ($1.t);
		}
	  | error
		{ $$ = error_mark_node; }
	;

ansi_raise_identifiers:
	  ansi_raise_identifier
		{ $$ = add_exception_specifier (NULL_TREE, $1, 1); }
	| ansi_raise_identifiers ',' ansi_raise_identifier
		{ $$ = add_exception_specifier ($1, $3, 1); }
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
		  $$ = build_nt (SCOPE_REF, $1, arg);
		}
	;

operator:
        OPERATOR
        {
	  saved_scopes = tree_cons (got_scope, got_object, saved_scopes);
	  TREE_LANG_FLAG_0 (saved_scopes) = looking_for_typename;
	  /* We look for conversion-type-id's in both the class and current
	     scopes, just as for ID in 'ptr->ID::'.  */
	  looking_for_typename = 1;
	  got_object = got_scope;
          got_scope = NULL_TREE;
	}
        ;

unoperator:
        { got_scope = TREE_PURPOSE (saved_scopes);
          got_object = TREE_VALUE (saved_scopes);
	  looking_for_typename = TREE_LANG_FLAG_0 (saved_scopes);
          saved_scopes = TREE_CHAIN (saved_scopes);
	}
        ;

operator_name:
	  operator '*' unoperator
		{ $$ = frob_opname (ansi_opname (MULT_EXPR)); }
	| operator '/' unoperator
		{ $$ = frob_opname (ansi_opname (TRUNC_DIV_EXPR)); }
	| operator '%' unoperator
		{ $$ = frob_opname (ansi_opname (TRUNC_MOD_EXPR)); }
	| operator '+' unoperator
		{ $$ = frob_opname (ansi_opname (PLUS_EXPR)); }
	| operator '-' unoperator
		{ $$ = frob_opname (ansi_opname (MINUS_EXPR)); }
	| operator '&' unoperator
		{ $$ = frob_opname (ansi_opname (BIT_AND_EXPR)); }
	| operator '|' unoperator
		{ $$ = frob_opname (ansi_opname (BIT_IOR_EXPR)); }
	| operator '^' unoperator
		{ $$ = frob_opname (ansi_opname (BIT_XOR_EXPR)); }
	| operator '~' unoperator
		{ $$ = frob_opname (ansi_opname (BIT_NOT_EXPR)); }
	| operator ',' unoperator
		{ $$ = frob_opname (ansi_opname (COMPOUND_EXPR)); }
	| operator ARITHCOMPARE unoperator
		{ $$ = frob_opname (ansi_opname ($2)); }
	| operator '<' unoperator
		{ $$ = frob_opname (ansi_opname (LT_EXPR)); }
	| operator '>' unoperator
		{ $$ = frob_opname (ansi_opname (GT_EXPR)); }
	| operator EQCOMPARE unoperator
		{ $$ = frob_opname (ansi_opname ($2)); }
	| operator ASSIGN unoperator
		{ $$ = frob_opname (ansi_assopname ($2)); }
	| operator '=' unoperator
		{ $$ = frob_opname (ansi_assopname (NOP_EXPR)); }
	| operator LSHIFT unoperator
		{ $$ = frob_opname (ansi_opname ($2)); }
	| operator RSHIFT unoperator
		{ $$ = frob_opname (ansi_opname ($2)); }
	| operator PLUSPLUS unoperator
		{ $$ = frob_opname (ansi_opname (POSTINCREMENT_EXPR)); }
	| operator MINUSMINUS unoperator
		{ $$ = frob_opname (ansi_opname (PREDECREMENT_EXPR)); }
	| operator ANDAND unoperator
		{ $$ = frob_opname (ansi_opname (TRUTH_ANDIF_EXPR)); }
	| operator OROR unoperator
		{ $$ = frob_opname (ansi_opname (TRUTH_ORIF_EXPR)); }
	| operator '!' unoperator
		{ $$ = frob_opname (ansi_opname (TRUTH_NOT_EXPR)); }
	| operator '?' ':' unoperator
		{ $$ = frob_opname (ansi_opname (COND_EXPR)); }
	| operator MIN_MAX unoperator
		{ $$ = frob_opname (ansi_opname ($2)); }
	| operator POINTSAT  unoperator %prec EMPTY
		{ $$ = frob_opname (ansi_opname (COMPONENT_REF)); }
	| operator POINTSAT_STAR  unoperator %prec EMPTY
		{ $$ = frob_opname (ansi_opname (MEMBER_REF)); }
	| operator LEFT_RIGHT unoperator
		{ $$ = frob_opname (ansi_opname (CALL_EXPR)); }
	| operator '[' ']' unoperator
		{ $$ = frob_opname (ansi_opname (ARRAY_REF)); }
	| operator NEW  unoperator %prec EMPTY
		{ $$ = frob_opname (ansi_opname (NEW_EXPR)); }
	| operator DELETE  unoperator %prec EMPTY
		{ $$ = frob_opname (ansi_opname (DELETE_EXPR)); }
	| operator NEW '[' ']' unoperator
		{ $$ = frob_opname (ansi_opname (VEC_NEW_EXPR)); }
	| operator DELETE '[' ']' unoperator
		{ $$ = frob_opname (ansi_opname (VEC_DELETE_EXPR)); }
	| operator type_specifier_seq conversion_declarator unoperator
		{ $$ = frob_opname (grokoptypename ($2.t, $3)); }
	| operator error unoperator
		{ $$ = frob_opname (ansi_opname (ERROR_MARK)); }
	;

/* The forced readahead in here is because we might be at the end of a
   line, and lineno won't be bumped until yylex absorbs the first token
   on the next line.  */
save_lineno:
		{ if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  $$ = lineno; }
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
