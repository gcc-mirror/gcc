/* Source code parsing and tree node generation for the GNU compiler
   for the Java(TM) language.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

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
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This file parses java source code and issues a tree node image
suitable for code generation (byte code and targeted CPU assembly
language).

The grammar conforms to the Java grammar described in "The Java(TM)
Language Specification. J. Gosling, B. Joy, G. Steele. Addison Wesley
1996, ISBN 0-201-63451-1"

The following modifications were brought to the original grammar:

method_body: added the rule '| block SC_TK'
constructor_declaration: added two rules to accept SC_TK. 
static_initializer: added the rule 'static block SC_TK'. 

Note: All the extra rules described above should go away when the
      empty_statement rule will work.

statement_nsi: 'nsi' should be read no_short_if.

Some rules have been modified to support JDK1.1 inner classes
definitions and other extensions.  */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "obstack.h"
#include "flags.h"
#include "java-tree.h"
#include "jcf.h"
#include "lex.h"
#include "parse.h"
#include "zipfile.h"

/* Number of error found so far. */
int java_error_count; 
/* Number of warning found so far. */
int java_warning_count;

/* The current parser context */
static struct parser_ctxt *ctxp;

/* binop_lookup maps token to tree_code. It is used where binary
   operations are involved and required by the parser. RDIV_EXPR
   covers both integral/floating point division. The code is changed
   once the type of both operator is worked out.  */

static enum tree_code binop_lookup[19] = 
  { 
    PLUS_EXPR, MINUS_EXPR, MULT_EXPR, RDIV_EXPR, TRUNC_MOD_EXPR,
    LSHIFT_EXPR, RSHIFT_EXPR, URSHIFT_EXPR, 
    BIT_AND_EXPR, BIT_XOR_EXPR, BIT_IOR_EXPR,
    TRUTH_ANDIF_EXPR, TRUTH_ORIF_EXPR,
    EQ_EXPR, NE_EXPR, GT_EXPR, GE_EXPR, LT_EXPR, LE_EXPR,
   };
#define BINOP_LOOKUP(VALUE) 						\
  binop_lookup [((VALUE) - PLUS_TK)%					\
		(sizeof (binop_lookup) / sizeof (binop_lookup[0]))]

/* Fake WFL used to report error message. It is initialized once if
   needed and reused with it's location information is overriden.  */
static tree wfl_operator = NULL_TREE;

/* The "$L" identifier we use to create labels.  */
static tree label_id;
%}

%union {
  tree node;
  int sub_token;
  struct {
    int token;
    int location;
  } operator;
  int value;
}

%pure_parser

/* Things defined here have to match the order of what's in the
   binop_lookup table.  */

%token   PLUS_TK         MINUS_TK        MULT_TK         DIV_TK    REM_TK
%token   LS_TK           SRS_TK          ZRS_TK
%token   AND_TK          XOR_TK          OR_TK
%token   BOOL_AND_TK BOOL_OR_TK 
%token   EQ_TK NEQ_TK GT_TK GTE_TK LT_TK LTE_TK

/* This maps to the same binop_lookup entry than the token above */

%token   PLUS_ASSIGN_TK  MINUS_ASSIGN_TK MULT_ASSIGN_TK DIV_ASSIGN_TK
%token   REM_ASSIGN_TK   
%token   LS_ASSIGN_TK    SRS_ASSIGN_TK   ZRS_ASSIGN_TK
%token   AND_ASSIGN_TK   XOR_ASSIGN_TK   OR_ASSIGN_TK


/* Modifier TOKEN have to be kept in this order. Don't scramble it */

%token   PUBLIC_TK       PRIVATE_TK         PROTECTED_TK
%token   STATIC_TK       FINAL_TK           SYNCHRONIZED_TK
%token   VOLATILE_TK     TRANSIENT_TK       NATIVE_TK
%token   PAD_TK          ABSTRACT_TK        MODIFIER_TK

/* Keep those two in order, too */
%token   DECR_TK INCR_TK

/* From now one, things can be in any order */

%token   DEFAULT_TK      IF_TK              THROW_TK
%token   BOOLEAN_TK      DO_TK              IMPLEMENTS_TK
%token   THROWS_TK       BREAK_TK           IMPORT_TK       
%token   ELSE_TK         INSTANCEOF_TK      RETURN_TK
%token   VOID_TK         CATCH_TK           INTERFACE_TK
%token   CASE_TK         EXTENDS_TK         FINALLY_TK
%token   SUPER_TK        WHILE_TK           CLASS_TK
%token   SWITCH_TK       CONST_TK           TRY_TK
%token   FOR_TK          NEW_TK             CONTINUE_TK
%token   GOTO_TK         PACKAGE_TK         THIS_TK

%token   BYTE_TK         SHORT_TK           INT_TK            LONG_TK
%token   CHAR_TK         INTEGRAL_TK

%token   FLOAT_TK        DOUBLE_TK          FP_TK

%token   ID_TK

%token   REL_QM_TK         REL_CL_TK NOT_TK  NEG_TK

%token   ASSIGN_ANY_TK   ASSIGN_TK
%token   OP_TK  CP_TK  OCB_TK  CCB_TK  OSB_TK  CSB_TK  SC_TK  C_TK DOT_TK

%token   STRING_LIT_TK   CHAR_LIT_TK        INT_LIT_TK        FP_LIT_TK
%token   TRUE_TK         FALSE_TK           BOOL_LIT_TK       NULL_TK

%type    <value>	modifiers MODIFIER_TK

%type    <node>		super ID_TK identifier
%type    <node>		name simple_name qualified_name
%type	 <node>		class_declaration type_declaration compilation_unit
			field_declaration method_declaration extends_interfaces
                        interfaces interface_type_list
                        interface_declaration class_member_declaration
                        import_declarations package_declaration 
                        type_declarations interface_body
			interface_member_declaration constant_declaration
			interface_member_declarations interface_type
			abstract_method_declaration interface_type_list
%type	 <node>		class_body_declaration class_member_declaration
			static_initializer constructor_declaration block
%type	 <node>		class_body_declarations
%type    <node>		class_or_interface_type class_type class_type_list
			constructor_declarator explicit_constructor_invocation
%type    <node>         dim_expr dim_exprs this_or_super

%type	 <node>		variable_declarator_id variable_declarator
			variable_declarators variable_initializer
			variable_initializers

%type	 <node>		class_body
%type	 <node>		block_statement local_variable_declaration_statement
			block_statements local_variable_declaration
%type	 <node>		statement statement_without_trailing_substatement
			labeled_statement if_then_statement label_decl
			if_then_else_statement while_statement for_statement
			statement_nsi labeled_statement_nsi do_statement
			if_then_else_statement_nsi while_statement_nsi
			for_statement_nsi statement_expression_list for_init
			for_update statement_expression expression_statement
			primary_no_new_array expression primary
			array_creation_expression array_type
			class_instance_creation_expression field_access
			method_invocation array_access something_dot_new
			argument_list postfix_expression while_expression 
			post_increment_expression post_decrement_expression
			unary_expression_not_plus_minus unary_expression
			pre_increment_expression pre_decrement_expression
			unary_expression_not_plus_minus cast_expression
			multiplicative_expression additive_expression
			shift_expression relational_expression 
			equality_expression and_expression 
			exclusive_or_expression inclusive_or_expression
			conditional_and_expression conditional_or_expression
			conditional_expression assignment_expression
			left_hand_side assignment for_header for_begin
			constant_expression do_statement_begin empty_statement
%type    <node>         return_statement break_statement continue_statement

%type    <operator>     ASSIGN_TK      MULT_ASSIGN_TK  DIV_ASSIGN_TK  
%type    <operator>     REM_ASSIGN_TK  PLUS_ASSIGN_TK  MINUS_ASSIGN_TK
%type    <operator>     LS_ASSIGN_TK   SRS_ASSIGN_TK   ZRS_ASSIGN_TK
%type    <operator>     AND_ASSIGN_TK  XOR_ASSIGN_TK   OR_ASSIGN_TK
%type    <operator>     ASSIGN_ANY_TK  assignment_operator
%token   <operator>     EQ_TK GTE_TK ZRS_TK SRS_TK GT_TK LTE_TK LS_TK 
%token   <operator>     BOOL_AND_TK AND_TK BOOL_OR_TK OR_TK INCR_TK PLUS_TK
%token   <operator>     DECR_TK MINUS_TK MULT_TK DIV_TK XOR_TK REM_TK NEQ_TK
%token   <operator>     NEG_TK REL_QM_TK REL_CL_TK NOT_TK LT_TK
%token   <operator>     OP_TK OSB_TK DOT_TK
%type    <operator>	THIS_TK SUPER_TK RETURN_TK BREAK_TK CONTINUE_TK

%type	 <node>		method_body 
	
%type    <node>		literal INT_LIT_TK FP_LIT_TK BOOL_LIT_TK CHAR_LIT_TK
			STRING_LIT_TK NULL_TK VOID_TK

%type	 <node>		IF_TK WHILE_TK FOR_TK

%type    <node>         formal_parameter_list formal_parameter
                        method_declarator method_header

%type	 <node>		primitive_type reference_type type
			BOOLEAN_TK INTEGRAL_TK FP_TK

%%
/* 19.2 Production from 2.3: The Syntactic Grammar  */
goal:
	compilation_unit
		{}
;

/* 19.3 Productions from 3: Lexical structure  */
literal:
	INT_LIT_TK
|	FP_LIT_TK
|	BOOL_LIT_TK
|	CHAR_LIT_TK
|       STRING_LIT_TK
|       NULL_TK
;

/* 19.4 Productions from 4: Types, Values and Variables  */
type:
	primitive_type
|	reference_type
;

primitive_type:
	INTEGRAL_TK
|	FP_TK
|	BOOLEAN_TK
;

reference_type:
	class_or_interface_type
|	array_type
;

class_or_interface_type:
	name
;

class_type:
	class_or_interface_type	/* Default rule */
;

interface_type:
	 class_or_interface_type
;

array_type:
	primitive_type OSB_TK CSB_TK
		{ 
		  $$ = build_java_array_type ($1, -1);
		  CLASS_LOADED_P ($$) = 1;
		}
|	name OSB_TK CSB_TK
		{ $$ = build_unresolved_array_type ($1); }
|	array_type OSB_TK CSB_TK
		{ $$ = build_unresolved_array_type ($1); }
|	primitive_type OSB_TK error
		{RULE ("']' expected"); RECOVER;}
|	array_type OSB_TK error
		{RULE ("']' expected"); RECOVER;}
;

/* 19.5 Productions from 6: Names  */
name:
	simple_name		/* Default rule */
|	qualified_name		/* Default rule */
;

simple_name:
	identifier		/* Default rule */
;

qualified_name:
	name DOT_TK identifier
		{ $$ = make_qualified_name ($1, $3, $2.location); }
;

identifier:
	ID_TK
;

/* 19.6: Production from 7: Packages  */
compilation_unit:
		{$$ = NULL;}
|	package_declaration
|	import_declarations
|	type_declarations
|       package_declaration import_declarations
|       package_declaration type_declarations
|       import_declarations type_declarations
|       package_declaration import_declarations type_declarations
;

import_declarations:
	import_declaration
		{
		  $$ = NULL;
		}
|	import_declarations import_declaration
		{
		  $$ = NULL;
		}
;

type_declarations:
	type_declaration
| 	type_declarations type_declaration
;

package_declaration:
	PACKAGE_TK name SC_TK
		{ ctxp->package = EXPR_WFL_NODE ($2); }
|	PACKAGE_TK error
		{yyerror ("Missing name"); RECOVER;}
|	PACKAGE_TK name error
		{yyerror ("';' expected"); RECOVER;}
;

import_declaration:
	single_type_import_declaration
|	type_import_on_demand_declaration
;

single_type_import_declaration:
	IMPORT_TK name SC_TK
		{
		  tree name = EXPR_WFL_NODE ($2), node, last_name;
		  int   i = IDENTIFIER_LENGTH (name)-1;
		  char *last = &IDENTIFIER_POINTER (name)[i];
		  while (last != IDENTIFIER_POINTER (name))
		    {
		      if (last [0] == '.')
			break;
		      last--;
		    }
		  last_name = get_identifier (++last);
		  if (IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (last_name))
		    {
		      tree err = find_name_in_single_imports (last_name);
		      if (err && err != name)
			parse_error_context
			  ($2, "Ambiguous class: `%s' and `%s'",
			   IDENTIFIER_POINTER (name), 
			   IDENTIFIER_POINTER (err));
		    }
		  else
		    {
		      IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (last_name) = 1;
		      node = build_tree_list ($2, last_name);
		      TREE_CHAIN (node) = ctxp->import_list;
		      ctxp->import_list = node;
		    }
		}
|	IMPORT_TK error
		{yyerror ("Missing name"); RECOVER;}
|	IMPORT_TK name error
		{yyerror ("';' expected"); RECOVER;}
;

type_import_on_demand_declaration:
	IMPORT_TK name DOT_TK MULT_TK SC_TK
		{
		  tree name = EXPR_WFL_NODE ($2);
		  tree node = build_tree_list ($2, NULL_TREE);
		  if (!IS_AN_IMPORT_ON_DEMAND_P (name))
		    {
		      read_import_dir ($2);
		      IS_AN_IMPORT_ON_DEMAND_P (name) = 1;
		    }
		  TREE_CHAIN (node) = ctxp->import_demand_list;
		  ctxp->import_demand_list = node;
		}
|	IMPORT_TK name DOT_TK error
		{yyerror ("'*' expected"); RECOVER;}
|	IMPORT_TK name DOT_TK MULT_TK error
		{yyerror ("';' expected"); RECOVER;}
;

type_declaration:
	class_declaration
		{
		  maybe_generate_clinit ();
		  $$ = $1;
		}
|	interface_declaration
|	SC_TK
		{ $$ = NULL; }
|	error
		{
		  YYERROR_NOW;
		  yyerror ("Class or interface declaration expected");
		}
;

/* 19.7 Shortened from the original:
   modifiers: modifier | modifiers modifier
   modifier: any of public...  */
modifiers:
	MODIFIER_TK
		{
		  $$ = (1 << $1);
		}
|	modifiers MODIFIER_TK
		{
		  int acc = (1 << $2);
		  if ($$ & acc)
		    parse_error_context 
		      (ctxp->modifier_ctx [$2], "Modifier `%s' declared twice",
		       java_accstring_lookup (acc));
		  else
		    {
		      $$ |= acc;
		    }
		}
;

/* 19.8.1 Production from $8.1: Class Declaration */
class_declaration:
	modifiers CLASS_TK identifier super interfaces
		{ create_class ($1, $3, $4, $5); }
	class_body
		{ 
		  $$ = $7;
		}
|	CLASS_TK identifier super interfaces 
		{ create_class (0, $2, $3, $4); }
	class_body
		{ 	
		  $$ = $6;
		}
|	modifiers CLASS_TK error
		{yyerror ("Missing class name"); RECOVER;}
|	CLASS_TK error
		{yyerror ("Missing class name"); RECOVER;}
|       CLASS_TK identifier error
		{if (!ctxp->class_err) yyerror ("'{' expected"); DRECOVER(class1);}
|       modifiers CLASS_TK identifier error
		{if (!ctxp->class_err) yyerror ("'{' expected"); RECOVER;}
;

super:
		{ $$ = NULL; }
|	EXTENDS_TK class_type
		{ $$ = $2; }
|	EXTENDS_TK class_type error
		{yyerror ("'{' expected"); ctxp->class_err=1;}
|	EXTENDS_TK error
		{yyerror ("Missing super class name"); ctxp->class_err=1;}
;

interfaces:
		{ $$ = NULL_TREE; }
|	IMPLEMENTS_TK interface_type_list
		{ $$ = $2; }
|	IMPLEMENTS_TK error
		{
		  ctxp->class_err=1;
		  yyerror ("Missing interface name"); 
		}
;

interface_type_list:
	interface_type
		{ 
		  ctxp->interface_number = 1;
		  $$ = build_tree_list ($1, NULL_TREE);
		}
|	interface_type_list C_TK interface_type
		{ 
		  ctxp->interface_number++;
		  $$ = chainon ($1, build_tree_list ($3, NULL_TREE));
		}
|	interface_type_list C_TK error
		{yyerror ("Missing interface name"); RECOVER;}
;

class_body:
	OCB_TK CCB_TK
		{ $$ = ctxp->current_parsed_class; }
|	OCB_TK class_body_declarations CCB_TK
		{ $$ = ctxp->current_parsed_class; }
;

class_body_declarations:
	class_body_declaration
|	class_body_declarations class_body_declaration
;

class_body_declaration:
	class_member_declaration
|	static_initializer
|	constructor_declaration
|	block			/* Added, JDK1.1, instance initializer */
;

class_member_declaration:
	field_declaration
|	method_declaration
|	class_declaration	/* Added, JDK1.1 inner classes */
|	interface_declaration	/* Added, JDK1.1 inner classes */
;

/* 19.8.2 Productions from 8.3: Field Declarations  */
field_declaration:
	type variable_declarators SC_TK
		{ register_fields (0, $1, $2); }
|	modifiers type variable_declarators SC_TK
		{
		  int acc_count = 0;

		  check_modifiers 
		    ("Illegal modifier `%s' for field declaration",
		     $1, FIELD_MODIFIERS);
		  check_modifiers_consistency ($1);
		  register_fields ($1, $2, $3);
		}
;

variable_declarators:
	/* Should we use build_decl_list () instead ? FIXME */
	variable_declarator	/* Default rule */
|	variable_declarators C_TK variable_declarator
		{ $$ = chainon ($1, $3); }
|	variable_declarators C_TK error
		{yyerror ("Missing term"); RECOVER;}
;

variable_declarator:
	variable_declarator_id
		{ $$ = build_tree_list ($1, NULL_TREE); }
|	variable_declarator_id ASSIGN_TK variable_initializer
		{ 
		  if (java_error_count)
		    $3 = NULL_TREE;
		  $$ = build_tree_list 
		    ($1, build_assignment ($2.token, $2.location, $1, $3));
		}
|	variable_declarator_id ASSIGN_TK error
		{
		  yyerror ("Missing variable initializer");
		  $$ = build_tree_list ($1, NULL_TREE);
		  RECOVER;
		}
|	variable_declarator_id ASSIGN_TK variable_initializer error
		{
		  yyerror ("';' expected");
		  $$ = build_tree_list ($1, NULL_TREE);
		  RECOVER;
		}
;

variable_declarator_id:
	identifier
|	variable_declarator_id OSB_TK CSB_TK
		{
		  $$ = NULL;	/* FIXME */
		}
|	identifier error
		{yyerror ("Invalid declaration"); DRECOVER(vdi);}
|	variable_declarator_id OSB_TK error
		{yyerror ("']' expected"); DRECOVER(vdi);}
|	variable_declarator_id CSB_TK error
		{yyerror ("Unbalanced ']'"); DRECOVER(vdi);}
;

variable_initializer:
	expression
|	array_initializer
		{ $$ = NULL; }
;

/* 19.8.3 Productions from 8.4: Method Declarations  */
method_declaration:
	method_header 
		{
		  current_function_decl = $1;
		  source_start_java_method (current_function_decl);
		}
	method_body
		{ 
		  BLOCK_EXPR_BODY 
		    (DECL_FUNCTION_BODY (current_function_decl)) = $3;
		  maybe_absorb_scoping_blocks ();
		  exit_block (); /* Exit function's body. */

		  /* Merge last line of the function with first line,
		     directly in the function decl. It will be used to
		     emit correct debug info. */
		  DECL_SOURCE_LINE_MERGE (current_function_decl,
					  ctxp->last_ccb_indent1);
		}
|	method_header error
		{YYNOT_TWICE yyerror ("'{' expected"); RECOVER;}
;

method_header:	
	type method_declarator throws
		{ $$ = method_header (0, $1, $2, NULL); }
|	VOID_TK method_declarator throws
		{ $$ = method_header (0, void_type_node, $2, NULL); }
|	modifiers type method_declarator throws
		{ $$ = method_header ($1, $2, $3, NULL); }
|	modifiers VOID_TK method_declarator throws
		{ $$ = method_header ($1, void_type_node, $3, NULL); }
|	type error
		{RECOVER;}
|	modifiers type error
		{RECOVER;}
|	VOID_TK error
		{yyerror ("Identifier expected"); RECOVER;}
|	modifiers VOID_TK error
		{yyerror ("Identifier expected"); RECOVER;}
|	modifiers error
		{
		  yyerror ("Invalid method declaration, return type required");
		  RECOVER;
		}
;

method_declarator:
	identifier OP_TK CP_TK
		{ $$ = method_declarator ($1, NULL_TREE); }
|	identifier OP_TK formal_parameter_list CP_TK
		{ $$ = method_declarator ($1, $3); }
|	method_declarator OSB_TK CSB_TK
		{
		  /* Issue a warning here: obsolete declaration. FIXME */
		  $$ = NULL;	/* FIXME */
		}
|	identifier OP_TK error
		{yyerror ("')' expected"); DRECOVER(method_declarator);}
|	method_declarator OSB_TK error
		{yyerror ("']' expected"); RECOVER;}
;

formal_parameter_list:
	formal_parameter
		{
		  ctxp->formal_parameter_number = 1;
		}
|	formal_parameter_list C_TK formal_parameter
		{
		  ctxp->formal_parameter_number += 1;
		  $$ = chainon ($1, $3);
		}
|	formal_parameter_list C_TK error
		{yyerror ("Missing formal parameter term"); RECOVER;}
;

formal_parameter:
	type variable_declarator_id
		{
		  $$ = build_tree_list ($2, $1);
		}
|	modifiers type variable_declarator_id /* Added, JDK1.1 final locals */
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		  $$ = NULL;	/* FIXME */
		}
|	type error
		{yyerror ("Missing identifier"); RECOVER;}
|	modifiers type error
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		  yyerror ("Missing identifier"); RECOVER;
		}
;

throws:
|	THROWS_TK class_type_list
|	THROWS_TK error
		{yyerror ("Missing class type term"); RECOVER;}
;

class_type_list:
	class_type
|	class_type_list C_TK class_type
|	class_type_list C_TK error
		{yyerror ("Missing class type term"); RECOVER;}
;

method_body:
	block
|	block SC_TK
|	SC_TK
		{ $$ = NULL_TREE; } /* Probably not the right thing to do. */
;

/* 19.8.4 Productions from 8.5: Static Initializers  */
static_initializer:
	static block
		{
		  RULE ("STATIC_INITIALIZER");
		}
|	static block SC_TK	/* Shouldn't be here. FIXME */
		{
		  RULE ("STATIC_INITIALIZER");
		}
;

static:				/* Test lval.sub_token here */
	MODIFIER_TK
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		}
;

/* 19.8.5 Productions from 8.6: Constructor Declarations  */
/* NOTE FOR FURTHER WORK ON CONSTRUCTORS:
   - If a forbidded modifier is found, the the error is either the use of
     a forbidded modifier for a constructor OR bogus attempt to declare a
     method without having specified the return type. FIXME */
constructor_declaration:
	constructor_declarator throws constructor_body
		{
		  RULE ("CONSTRUCTOR_DECLARATION");
		}
|	modifiers constructor_declarator throws constructor_body
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		  RULE ("CONSTRUCTOR_DECLARATION (modifier)");
		}
/* extra SC_TK, FIXME */
|	constructor_declarator throws constructor_body SC_TK
		{
		  RULE ("CONSTRUCTOR_DECLARATION");
		}
/* extra SC_TK, FIXME */
|	modifiers constructor_declarator throws constructor_body SC_TK
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		  RULE ("CONSTRUCTOR_DECLARATION (modifier)");
		}
/* I'm not happy with the SC_TK addition. It isn't in the grammer and should
   probably be matched by and empty statement. But it doesn't work. FIXME */
;

constructor_declarator:
	simple_name OP_TK CP_TK
|	simple_name OP_TK formal_parameter_list CP_TK
;

constructor_body:
	OCB_TK CCB_TK
|	OCB_TK explicit_constructor_invocation CCB_TK
|	OCB_TK block_statements CCB_TK
|       OCB_TK explicit_constructor_invocation block_statements CCB_TK
;

/* Error recovery for that rule moved down expression_statement: rule.  */
explicit_constructor_invocation:
	this_or_super OP_TK CP_TK SC_TK
|	this_or_super OP_TK argument_list CP_TK SC_TK
        /* Added, JDK1.1 inner classes. Modified because the rule
	   'primary' couldn't work.  */
|	name DOT_TK SUPER_TK OP_TK argument_list CP_TK SC_TK
|	name DOT_TK SUPER_TK OP_TK CP_TK SC_TK
		{RULE ("explicit_constructor_invocation (X.super)");}
;

this_or_super:			/* Added, simplifies error diagnostics */
	THIS_TK
		{
		  tree wfl = build_wfl_node (this_identifier_node, input_filename, 0, 0);
		  EXPR_WFL_LINECOL (wfl) = $1.location;
		  $$ = wfl;
		}
|	SUPER_TK
		{
		  tree wfl = build_wfl_node (super_identifier_node, input_filename, 0, 0);
		  EXPR_WFL_LINECOL (wfl) = $1.location;
		  $$ = wfl;
		}
;

/* 19.9 Productions from 9: Interfaces  */
/* 19.9.1 Productions from 9.1: Interfaces Declarations  */
interface_declaration:
	INTERFACE_TK identifier
		{ create_interface (0, $2, NULL_TREE); }
	interface_body
		{
		  $$ = $4;
		}
|	modifiers INTERFACE_TK identifier
		{ create_interface ($1, $3, NULL_TREE); }
	interface_body
		{
		  $$ = $5;
		}
|	INTERFACE_TK identifier extends_interfaces
		{ create_interface (0, $2, $3);	}
	interface_body
		{
		  $$ = $5;
		}
|	modifiers INTERFACE_TK identifier extends_interfaces
		{ create_interface ($1, $3, $4); }
	interface_body
		{
		  $$ = $6;
		}
|	INTERFACE_TK identifier error
		{yyerror ("(here)'{' expected"); RECOVER;}
|	modifiers INTERFACE_TK identifier error
		{yyerror ("(there)'{' expected"); RECOVER;}
;

extends_interfaces:
	EXTENDS_TK interface_type
		{ 
		  ctxp->interface_number = 1;
		  $$ = build_tree_list ($2, NULL_TREE);
		}
|	extends_interfaces C_TK interface_type
		{ 
		  ctxp->interface_number++;
		  $$ = chainon ($1, build_tree_list ($3, NULL_TREE));
		}
|	EXTENDS_TK error
		{yyerror ("Invalid interface type"); RECOVER;}
|	extends_interfaces C_TK error
		{yyerror ("Missing term"); RECOVER;}
;

interface_body:
	OCB_TK CCB_TK
		{ $$ = NULL_TREE; }
|	OCB_TK interface_member_declarations CCB_TK
		{ $$ = NULL_TREE; }
;

interface_member_declarations:
	interface_member_declaration
|	interface_member_declarations interface_member_declaration
;

interface_member_declaration:
	constant_declaration
|	abstract_method_declaration
|	class_declaration	/* Added, JDK1.1 inner classes */
|	interface_declaration	/* Added, JDK1.1 inner classes */
;

constant_declaration:
	field_declaration
;

abstract_method_declaration:
	method_header SC_TK
		{ 
		  check_abstract_method_header ($1);
		  current_function_decl = NULL_TREE; /* FIXME ? */
		}
|	method_header error
		{yyerror ("';' expected"); RECOVER;}
;

/* 19.10 Productions from 10: Arrays  */
array_initializer:
	OCB_TK CCB_TK
		{
		  RULE ("ARRAY_INITIALIZER (empty)");
		}
|	OCB_TK variable_initializers CCB_TK
		{
		  RULE ("ARRAY_INITIALIZER (variable)");
		}
|	OCB_TK C_TK CCB_TK
		{
		  RULE ("ARRAY_INITIALIZER (,)");
		}
|	OCB_TK variable_initializers C_TK CCB_TK
		{
		  RULE ("ARRAY_INITIALIZER (variable, ,)");
		}
;

variable_initializers:
	variable_initializer
|	variable_initializers C_TK variable_initializer
|	variable_initializers C_TK error
		{yyerror ("Missing term"); RECOVER;}
;

/* 19.11 Production from 14: Blocks and Statements  */
block:
	OCB_TK CCB_TK
		{ $$ = size_zero_node; }
|	OCB_TK 
		{ enter_block (); }
	block_statements
	CCB_TK
		{ 
		  maybe_absorb_scoping_blocks ();
		  $$ = exit_block ();
		}
;

block_statements:
	block_statement
|	block_statements block_statement
;

block_statement:
	local_variable_declaration_statement
|	statement
		{ $$ = java_method_add_stmt (current_function_decl, $1); }
|	class_declaration	/* Added, JDK1.1 inner classes */
;

local_variable_declaration_statement:
	local_variable_declaration SC_TK /* Can't catch missing ';' here */
;

local_variable_declaration:
	type variable_declarators
		{ declare_local_variables (0, $1, $2); }
|	modifiers type variable_declarators /* Added, JDK1.1 final locals */
		{ declare_local_variables ($1, $2, $3); }
;

statement:
	statement_without_trailing_substatement
|	labeled_statement
		{ RULE ("STATEMENT (labeled)"); }
|	if_then_statement
		{ RULE ("STATEMENT (if-then)"); }
|	if_then_else_statement
		{ RULE ("STATEMENT (if-then-else)"); }
|	while_statement
		{ RULE ("STATEMENT (while)"); }
|	for_statement
		{ 
		  /* If the for loop is unlabeled, we must return the
		     block it was defined it. It our last chance to
		     get a hold on it. */
		  if (!LOOP_HAS_LABEL_P ($$))
		    $$ = exit_block ();
		}
;

statement_nsi:
	statement_without_trailing_substatement
|	labeled_statement_nsi
		{ RULE ("NSI STATEMENT (labeled)"); }
|	if_then_else_statement_nsi
		{ RULE ("NSI STATEMENT (if-then-else)"); }
|	while_statement_nsi
		{ RULE ("NSI STATEMENT (while)"); }
|	for_statement_nsi
		{ RULE ("NSI STATEMENT (for)"); }
;

statement_without_trailing_substatement:
	block
		{ RULE ("STATEMENT (block)"); }
|	empty_statement
		{ RULE ("STATEMENT (empty)"); }
|	expression_statement
		{ RULE ("STATEMENT (expression)"); }
|	switch_statement
		{ RULE ("STATEMENT (switch)"); }
|	do_statement
		{ RULE ("STATEMENT (do)"); }
|	break_statement
		{ RULE ("STATEMENT (break)"); }
|	continue_statement
		{ RULE ("STATEMENT (continue)"); }
|	return_statement
|	synchronized_statement
		{ RULE ("STATEMENT (synchronized)"); }
|	throw_statement
		{ RULE ("STATEMENT (throw)"); }
|	try_statement
		{ RULE ("STATEMENT (try)"); }
;

empty_statement:
	SC_TK
		{ $$ = size_zero_node; }
;

label_decl:
	identifier REL_CL_TK
		{
		  $$ = build_labeled_block (EXPR_WFL_LINECOL ($1), 
					    EXPR_WFL_NODE ($1), $1);
		  pushlevel (2);
		  push_labeled_block ($$);
		  PUSH_LABELED_BLOCK ($$);
		}
;

labeled_statement:
	label_decl statement
		{ 
		  $$ = complete_labeled_statement ($1, $2);
		  pop_labeled_block ();
		  POP_LABELED_BLOCK ();
		}
|	identifier error
		{yyerror ("':' expected"); RECOVER;}
;

labeled_statement_nsi:
	label_decl statement_nsi
		{ 
		  $$ = complete_labeled_statement ($1, $2);
		  pop_labeled_block ();
		  POP_LABELED_BLOCK ();
		}
;

/* We concentrate here a bunch of error handling rules that we couldn't write
   earlier, because expression_statement catches a missing ';'.  */
expression_statement:
	statement_expression SC_TK
		{
		  /* We have a statement. Generate a WFL around it so
		     we can debug it */
		  $$ = build_expr_wfl ($1, input_filename, lineno, 0);
		  /* We know we have a statement, so set the debug
                     info to be eventually generate here. */
		  $$ = JAVA_MAYBE_GENERATE_DEBUG_INFO ($$);
		}
|	error SC_TK 
		{
		  if (ctxp->prevent_ese != lineno)
		    yyerror ("Invalid expression statement");
		  DRECOVER (expr_stmt);
		}
|	error OCB_TK
		{
		  if (ctxp->prevent_ese != lineno)
		    yyerror ("Invalid expression statement");
		  DRECOVER (expr_stmt);
		}
|	error CCB_TK
		{
		  if (ctxp->prevent_ese != lineno)
		    yyerror ("Invalid expression statement");
		  DRECOVER (expr_stmt);
		}
|       this_or_super OP_TK error
		{yyerror ("')' expected"); RECOVER;}
|       this_or_super OP_TK CP_TK error
		{yyerror ("';' expected"); RECOVER;}
|       this_or_super OP_TK argument_list error
		{yyerror ("')' expected"); RECOVER;}
|       this_or_super OP_TK argument_list CP_TK error
		{yyerror ("';' expected"); RECOVER;}
|	name DOT_TK SUPER_TK error
		{yyerror ("'(' expected"); RECOVER;}
|	name DOT_TK SUPER_TK OP_TK error
		{yyerror ("')' expected"); RECOVER;}
|	name DOT_TK SUPER_TK OP_TK argument_list error
		{yyerror ("')' expected"); RECOVER;}
|	name DOT_TK SUPER_TK OP_TK argument_list CP_TK error
		{yyerror ("';' expected"); RECOVER;}
|	name DOT_TK SUPER_TK OP_TK CP_TK error
		{yyerror ("';' expected"); RECOVER;}
;

statement_expression: 
	assignment
|	pre_increment_expression
		{
		  RULE ("++INCREMENT");
		}
|	pre_decrement_expression
		{
		  RULE ("--DECREMENT");
		}
|	post_increment_expression
		{
		  RULE ("INCREMENT++");
		}
|	post_decrement_expression
		{
		  RULE ("DECREMENT--");
		}
|	method_invocation
|	class_instance_creation_expression
		{
		  RULE ("INSTANCE CREATION");
		}
;

if_then_statement:
	IF_TK OP_TK expression CP_TK statement
	{ $$ = build_if_else_statement ($2.location, $3, $5, NULL_TREE); }
|	IF_TK error
		{yyerror ("'(' expected"); RECOVER;}
|	IF_TK OP_TK error
		{yyerror ("Missing term"); RECOVER;}
|	IF_TK OP_TK expression error
		{yyerror ("')' expected"); RECOVER;}
;

if_then_else_statement:
	IF_TK OP_TK expression CP_TK statement_nsi ELSE_TK statement
	{ $$ = build_if_else_statement ($2.location, $3, $5, $7); }
;

if_then_else_statement_nsi:
	IF_TK OP_TK expression CP_TK statement_nsi ELSE_TK statement_nsi
	{ $$ = build_if_else_statement ($2.location, $3, $5, $7); }
;

switch_statement:
	SWITCH_TK OP_TK expression CP_TK switch_block
|	SWITCH_TK error
		{yyerror ("'(' expected"); RECOVER;}
|	SWITCH_TK OP_TK error
		{yyerror ("Missing term or ')'"); DRECOVER(switch_statement);}
|	SWITCH_TK OP_TK expression CP_TK error
		{yyerror ("'{' expected"); RECOVER;}
;

switch_block:
	OCB_TK CCB_TK
|	OCB_TK switch_labels CCB_TK
|	OCB_TK switch_block_statement_groups CCB_TK
|	OCB_TK switch_block_statement_groups switch_labels CCB_TK
;

switch_block_statement_groups: 
	switch_block_statement_group
|	switch_block_statement_groups switch_block_statement_group
;

switch_block_statement_group:
	switch_labels block_statements
;


switch_labels:
	switch_label
|	switch_labels switch_label
;

switch_label:
	CASE_TK constant_expression REL_CL_TK
|	DEFAULT_TK REL_CL_TK
|	CASE_TK error
		{yyerror ("Missing or invalid constant expression"); RECOVER;}
|	CASE_TK constant_expression error
		{yyerror ("':' expected"); RECOVER;}
|	DEFAULT_TK error
		{yyerror ("':' expected"); RECOVER;}
;

while_expression:
	WHILE_TK OP_TK expression CP_TK
		{ 
		  tree body = build_loop_body ($2.location, $3, 0);
		  $$ = build_new_loop (body);
		}
;

while_statement:
	while_expression statement
		{ $$ = complete_loop_body (0, NULL_TREE, $2, 0); }
|	WHILE_TK error
		{YYERROR_NOW; yyerror ("'(' expected"); RECOVER;}
|	WHILE_TK OP_TK error
		{yyerror ("Missing term and ')' expected"); RECOVER;}
|	WHILE_TK OP_TK expression error
		{yyerror ("')' expected"); RECOVER;}
;

while_statement_nsi:
	while_expression statement_nsi
		{ $$ = complete_loop_body (0, NULL_TREE, $2, 0); }
;

do_statement_begin:
	DO_TK
		{ 
		  tree body = build_loop_body (0, NULL_TREE, 1);
		  $$ = build_new_loop (body);
		}
	/* Need error handing here. FIXME */
;

do_statement: 
	do_statement_begin statement WHILE_TK OP_TK expression CP_TK SC_TK
		{ $$ = complete_loop_body ($4.location, $5, $2, 1); }
;

for_statement:
	for_begin SC_TK expression SC_TK for_update CP_TK statement
		{ $$ = complete_for_loop (EXPR_WFL_LINECOL ($3), $3, $5, $7);}
|	for_begin SC_TK SC_TK for_update CP_TK statement
		{ 
		  $$ = complete_for_loop (0, NULL_TREE, $4, $6);
		  /* We have not condition, so we get rid of the EXIT_EXPR */
		  LOOP_EXPR_BODY_CONDITION_EXPR (LOOP_EXPR_BODY ($$), 0) = 
		    size_zero_node;
		}
|	for_begin SC_TK error
		{yyerror ("Invalid control expression"); RECOVER;}
|	for_begin SC_TK expression SC_TK error
		{yyerror ("Invalid update expression"); RECOVER;}
|	for_begin SC_TK SC_TK error
		{yyerror ("Invalid update expression"); RECOVER;}
;

for_statement_nsi:
	for_begin SC_TK expression SC_TK for_update CP_TK statement_nsi
		{ $$ = complete_for_loop (EXPR_WFL_LINECOL ($3), $3, $5, $7);}
|	for_begin SC_TK SC_TK for_update CP_TK statement_nsi
		{ 
		  $$ = complete_for_loop (0, NULL_TREE, $4, $6);
		  /* We have not condition, so we get rid of the EXIT_EXPR */
		  LOOP_EXPR_BODY_CONDITION_EXPR (LOOP_EXPR_BODY ($$), 0) = 
		    size_zero_node;
		}
;

for_header:
	FOR_TK OP_TK
		{ 
		  /* This scope defined for local variable that may be
                     defined within the scope of the for loop */
		  enter_block (); 
		}
|	FOR_TK error
		{yyerror ("'(' expected"); DRECOVER(for_1);}
|	FOR_TK OP_TK error
		{yyerror ("Invalid init statement"); RECOVER;}
;

for_begin:
	for_header for_init
		{ 
		  /* We now declare the loop body. The loop is
                     declared as a for loop. */
		  tree body = build_loop_body (0, NULL_TREE, 0);
		  $$ =  build_new_loop (body);
		  IS_FOR_LOOP_P ($$) = 1;
		  /* The loop is added to the current block the for
                     statement is defined within */
		  java_method_add_stmt (current_function_decl, $$);
		}
;
for_init:			/* Can be empty */
		{ $$ = size_zero_node; }
|	statement_expression_list
		{ 
		  /* Init statement recorded within the previously
                     defined block scope */
		  $$ = java_method_add_stmt (current_function_decl, $1);
		}
|	local_variable_declaration
		{ 
		  /* Local variable are recorded within the previously
		     defined block scope */
		  $$ = NULL_TREE;
		}
|	statement_expression_list error
		{yyerror ("';' expected"); DRECOVER(for_init_1);}
;

for_update:			/* Can be empty */
		{$$ = size_zero_node;}
|	statement_expression_list
		{ $$ = build_debugable_stmt (BUILD_LOCATION (), $1); }
;

statement_expression_list:
	statement_expression
		{ $$ = add_stmt_to_compound (NULL_TREE, NULL_TREE, $1); }
|	statement_expression_list C_TK statement_expression
		{ $$ = add_stmt_to_compound ($1, NULL_TREE, $3); }
|	statement_expression_list C_TK error
		{yyerror ("Missing term"); RECOVER;}
;

break_statement:
	BREAK_TK SC_TK
		{ $$ = build_bc_statement ($1.location, 1, NULL_TREE); }
|	BREAK_TK identifier SC_TK
		{ $$ = build_bc_statement ($1.location, 1, $2); }
|	BREAK_TK error
		{yyerror ("Missing term"); RECOVER;}
|	BREAK_TK identifier error
		{yyerror ("';' expected"); RECOVER;}
;

continue_statement:
	CONTINUE_TK SC_TK
		{ $$ = build_bc_statement ($1.location, 0, NULL_TREE); }
|       CONTINUE_TK identifier SC_TK
		{ $$ = build_bc_statement ($1.location, 0, $2); }
|	CONTINUE_TK error
		{yyerror ("Missing term"); RECOVER;}
|	CONTINUE_TK identifier error
		{yyerror ("';' expected"); RECOVER;}
;

return_statement:
	RETURN_TK SC_TK
		{ $$ = build_return ($1.location, NULL_TREE); }
|	RETURN_TK expression SC_TK
		{ $$ = build_return ($1.location, $2); }
|	RETURN_TK error
		{yyerror ("Missing term"); RECOVER;}
|	RETURN_TK expression error
		{yyerror ("';' expected"); RECOVER;}
;

throw_statement:
	THROW_TK expression SC_TK
|	THROW_TK error
		{yyerror ("Missing term"); RECOVER;}
|	THROW_TK expression error
		{yyerror ("';' expected"); RECOVER;}
;

synchronized_statement:
	synchronized OP_TK expression CP_TK block
|	synchronized OP_TK expression CP_TK error
		{yyerror ("'{' expected"); RECOVER;}
|	synchronized error
		{yyerror ("'(' expected"); RECOVER;}
|	synchronized OP_TK error CP_TK
		{yyerror ("Missing term"); RECOVER;}
|	synchronized OP_TK error
		{yyerror ("Missing term"); RECOVER;}
;

synchronized:			/* Test lval.sub_token here */
	MODIFIER_TK
		{
		  SOURCE_FRONTEND_DEBUG (("Modifiers: %d", $1));
		}
;

try_statement:
	TRY_TK block catches
|	TRY_TK block finally
|	TRY_TK block catches finally
|	TRY_TK error
		{yyerror ("'{' expected"); DRECOVER (try_statement);}
;

catches:
	catch_clause
|	catches catch_clause
;

catch_clause:
	CATCH_TK OP_TK formal_parameter CP_TK block
|	CATCH_TK error
		{yyerror ("'(' expected"); RECOVER;}
|	CATCH_TK OP_TK error CP_TK /* That's for () */
		{yyerror ("Missing term"); DRECOVER (1);}
|	CATCH_TK OP_TK error 
		{yyerror ("Missing term"); DRECOVER (2);}
;

finally:
	FINALLY_TK block
|	FINALLY_TK error
		{yyerror ("'{' expected"); RECOVER; }
;

/* 19.12 Production from 15: Expressions  */
primary:
	primary_no_new_array
|	array_creation_expression
;

primary_no_new_array:
	literal
|	THIS_TK
		{ $$ = build_this ($1.location); }
|	OP_TK expression CP_TK
		{$$ = $2;}
|	class_instance_creation_expression
|	field_access
|	method_invocation
|	array_access
	/* type DOT_TK CLASS_TK doens't work. So we split the rule
	   'type' into its components. Missing is something for array,
	   which will complete the reference_type part. FIXME */
|	name DOT_TK CLASS_TK	       /* Added, JDK1.1 class literals */
|	primitive_type DOT_TK CLASS_TK /* Added, JDK1.1 class literals */
|	VOID_TK DOT_TK CLASS_TK	       /* Added, JDK1.1 class literals */
        /* Added, JDK1.1 inner classes. Documentation is wrong
           refering to a 'ClassName' (class_name) rule that doesn't
           exist. Used name instead.  */
|	name DOT_TK THIS_TK
|	OP_TK expression error 
		{yyerror ("')' expected"); RECOVER;}
|	name DOT_TK error
		{yyerror ("'class' or 'this' expected" ); RECOVER;}
|	primitive_type DOT_TK error
		{yyerror ("'class' expected" ); RECOVER;}
|	VOID_TK DOT_TK error
		{yyerror ("'class' expected" ); RECOVER;}
;

class_instance_creation_expression:
	NEW_TK class_type OP_TK argument_list CP_TK
		{
		  $$ = build_method_invocation ($2, $4);
		  TREE_SET_CODE ($$, JAVA_NEW_CLASS_EXPR);
		}
|	NEW_TK class_type OP_TK CP_TK
		{
		  $$ = build_method_invocation ($2, NULL_TREE);
		  TREE_SET_CODE ($$, JAVA_NEW_CLASS_EXPR);
		}
        /* Added, JDK1.1 inner classes but modified to use
           'class_type' instead of 'TypeName' (type_name) mentionned
           in the documentation but doesn't exist. */
|	NEW_TK class_type OP_TK argument_list CP_TK class_body
{$$ = $2;}
|	NEW_TK class_type OP_TK CP_TK class_body         
{$$ = $2;}
        /* Added, JDK1.1 inner classes, modified to use name or
	   primary instead of primary solely which couldn't work in
	   all situations.  */
|	something_dot_new identifier OP_TK CP_TK
|	something_dot_new identifier OP_TK CP_TK class_body
|	something_dot_new identifier OP_TK argument_list CP_TK
|	something_dot_new identifier OP_TK argument_list CP_TK class_body
|	NEW_TK error SC_TK 
		{yyerror ("'(' expected"); DRECOVER(new_1);}
|	NEW_TK class_type error
		{yyerror ("'(' expected"); RECOVER;}
|	NEW_TK class_type OP_TK error
		{yyerror ("')' or term expected"); RECOVER;}
|	NEW_TK class_type OP_TK argument_list error
		{yyerror ("')' expected"); RECOVER;}
|	something_dot_new error
		{YYERROR_NOW; yyerror ("Identifier expected"); RECOVER;}
|	something_dot_new identifier error
		{yyerror ("'(' expected"); RECOVER;}
;

something_dot_new:		/* Added, not part of the specs. */
	name DOT_TK NEW_TK
|	primary DOT_TK NEW_TK
;

argument_list:
	expression
		{ 
		  $$ = tree_cons (NULL_TREE, $1, NULL_TREE);
		  ctxp->formal_parameter_number = 1; 
		}
|	argument_list C_TK expression
		{
		  ctxp->formal_parameter_number += 1;
		  $$ = tree_cons (NULL_TREE, $3, $1);
		}
|	argument_list C_TK error
		{yyerror ("Missing term"); RECOVER;}
;

array_creation_expression:
	NEW_TK primitive_type dim_exprs
		{ $$ = build_newarray_node ($2, $3, 0); }
|	NEW_TK class_or_interface_type dim_exprs
		{ $$ = build_newarray_node ($2, $3, 0); }
|	NEW_TK primitive_type dim_exprs dims
		{ $$ = build_newarray_node ($2, $3, ctxp->osb_number); }
|	NEW_TK class_or_interface_type dim_exprs dims
		{ $$ = build_newarray_node ($2, $3, ctxp->osb_number); }
        /* Added, JDK1.1 anonymous array. Initial documentation rule
           modified */
|	NEW_TK class_or_interface_type dims array_initializer
{$$ = $2;}
|	NEW_TK primitive_type dims array_initializer
{$$ = $2;}
|	NEW_TK error CSB_TK
		{yyerror ("'[' expected"); DRECOVER ("]");}
|	NEW_TK error OSB_TK
		{yyerror ("']' expected"); RECOVER;}
;

dim_exprs:
	dim_expr
		{ $$ = build_tree_list (NULL_TREE, $1); }
|	dim_exprs dim_expr
		{ $$ = tree_cons (NULL_TREE, $2, $$); }
;

dim_expr:
	OSB_TK expression CSB_TK
		{ 
		  EXPR_WFL_LINECOL ($2) = $1.location;
		  $$ = $2;
		}
|	OSB_TK expression error
		{yyerror ("']' expected"); RECOVER;}
|	OSB_TK error
		{
		  yyerror ("Missing term");
		  yyerror ("']' expected");
		  RECOVER;
		}
;

dims:				
	OSB_TK CSB_TK
		{ ctxp->osb_number = 1; }
|	dims OSB_TK CSB_TK
		{ ctxp->osb_number++; }
|	dims OSB_TK error
		{ yyerror ("']' expected"); RECOVER;}
;

field_access:
	primary DOT_TK identifier
		{ $$ = make_qualified_primary ($1, $3, $2.location); }
|	SUPER_TK DOT_TK identifier
		{
		  tree super_wfl = 
		    build_wfl_node (super_identifier_node, input_filename, 0, 0);
		  EXPR_WFL_LINECOL (super_wfl) = $1.location;
		  $$ = make_qualified_name (super_wfl, $3, $2.location);
		}
|	SUPER_TK error
		{yyerror ("Field expected"); DRECOVER (super_field_acces);}
;

method_invocation:
	name OP_TK CP_TK
		{ $$ = build_method_invocation ($1, NULL_TREE); }
|	name OP_TK argument_list CP_TK
		{ $$ = build_method_invocation ($1, $3); }
|	primary DOT_TK identifier OP_TK CP_TK
		{ 
		  tree invok = build_method_invocation ($3, NULL_TREE);
		  $$ = make_qualified_primary ($1, invok, $2.location);
		}
|	primary DOT_TK identifier OP_TK argument_list CP_TK
		{ 
		  tree invok = build_method_invocation ($3, $5);
		  $$ = make_qualified_primary ($1, invok, $2.location);
		}
|	SUPER_TK DOT_TK identifier OP_TK CP_TK
		{
		  tree invok;
		  tree wfl = build_wfl_node (super_identifier_node, input_filename, 0, 0);
		  EXPR_WFL_LINECOL (wfl) = $1.location;
		  invok = build_method_invocation ($3, NULL_TREE);
		  $$ = make_qualified_primary (wfl, invok, $2.location);
		}
|	SUPER_TK DOT_TK identifier OP_TK argument_list CP_TK
		{
		  tree invok;
		  tree wfl = build_wfl_node (super_identifier_node, input_filename, 0, 0);
		  EXPR_WFL_LINECOL (wfl) = $1.location;
		  invok = build_method_invocation ($3, $5);
		  $$ = make_qualified_primary (wfl, invok, $2.location);
		}
        /* Screws up thing. I let it here until I'm convinced it can
           be removed. FIXME
|	primary DOT_TK error
		{yyerror ("'(' expected"); DRECOVER(bad);} */
|	SUPER_TK DOT_TK error CP_TK
		{ yyerror ("'(' expected"); DRECOVER (method_invocation); }
|	SUPER_TK DOT_TK error DOT_TK
		{ yyerror ("'(' expected"); DRECOVER (method_invocation); }
;

array_access:
	name OSB_TK expression CSB_TK
		{ $$ = build_array_ref ($2.location, $1, $3); }
|	primary_no_new_array OSB_TK expression CSB_TK
		{ $$ = build_array_ref ($2.location, $1, $3); }
|	name OSB_TK error
		{
		  yyerror ("Missing term and ']' expected");
		  DRECOVER(array_access);
		}
|	name OSB_TK expression error
		{
		  yyerror ("']' expected");
		  DRECOVER(array_access);
		}
|	primary_no_new_array OSB_TK error
		{
		  yyerror ("Missing term and ']' expected");
		  DRECOVER(array_access);
		}
|	primary_no_new_array OSB_TK expression error
		{
		  yyerror ("']' expected");
		  DRECOVER(array_access);
		}
;

postfix_expression:
	primary
|	name
|	post_increment_expression
|	post_decrement_expression
;

post_increment_expression:
	postfix_expression INCR_TK
		{ $$ = build_incdec ($2.token, $2.location, $1, 1); }
;

post_decrement_expression:
	postfix_expression DECR_TK
		{ $$ = build_incdec ($2.token, $2.location, $1, 1); }
;

unary_expression:
	pre_increment_expression
|	pre_decrement_expression
|	PLUS_TK unary_expression
		{$$ = build_unaryop ($1.token, $1.location, $2); }
|	MINUS_TK unary_expression
		{$$ = build_unaryop ($1.token, $1.location, $2); }
|	unary_expression_not_plus_minus
|	PLUS_TK error
		{yyerror ("Missing term"); RECOVER}
|	MINUS_TK error
		{yyerror ("Missing term"); RECOVER}
;

pre_increment_expression:
	INCR_TK unary_expression
		{$$ = build_incdec ($1.token, $1.location, $2, 0); }
|	INCR_TK error
		{yyerror ("Missing term"); RECOVER}
;

pre_decrement_expression:
	DECR_TK unary_expression
		{$$ = build_incdec ($1.token, $1.location, $2, 0); }
|	DECR_TK error
		{yyerror ("Missing term"); RECOVER}
;

unary_expression_not_plus_minus:
	postfix_expression
|	NOT_TK unary_expression
		{$$ = build_unaryop ($1.token, $1.location, $2); }
|	NEG_TK unary_expression
 		{$$ = build_unaryop ($1.token, $1.location, $2); }
|	cast_expression
|       NOT_TK error
		{yyerror ("Missing term"); RECOVER}
|       NEG_TK error
		{yyerror ("Missing term"); RECOVER}
;

cast_expression:		/* Error handling here is potentially weak */
	OP_TK primitive_type dims CP_TK unary_expression
		{ 
		  tree type = $2;
		  while (ctxp->osb_number--)
		    type = build_java_array_type (type, -1);
		  $$ = build_cast ($1.location, type, $5); 
		}
|	OP_TK primitive_type CP_TK unary_expression
		{ $$ = build_cast ($1.location, $2, $4); }
|	OP_TK expression CP_TK unary_expression_not_plus_minus
		{ $$ = build_cast ($1.location, $2, $4); }
|	OP_TK name dims CP_TK unary_expression_not_plus_minus
		{ 
		  char *ptr;
		  while (ctxp->osb_number--)
		    obstack_1grow (&temporary_obstack, '[');
		  obstack_grow0 (&temporary_obstack, 
				 IDENTIFIER_POINTER (EXPR_WFL_NODE ($2)),
				 IDENTIFIER_LENGTH (EXPR_WFL_NODE ($2)));
		  ptr = obstack_finish (&temporary_obstack);
		  EXPR_WFL_NODE ($2) = get_identifier (ptr);
		  $$ = build_cast ($1.location, $2, $5);
		}
|	OP_TK primitive_type OSB_TK error
		{yyerror ("']' expected, invalid type expression");}
|       OP_TK error
		{
	          if (ctxp->prevent_ese != lineno)
		    yyerror ("Invalid type expression"); RECOVER;
		  RECOVER;
		}
|	OP_TK primitive_type dims CP_TK error
		{yyerror ("Missing term"); RECOVER;}
|	OP_TK primitive_type CP_TK error
		{yyerror ("Missing term"); RECOVER;}
|	OP_TK name dims CP_TK error
		{yyerror ("Missing term"); RECOVER;}
;

multiplicative_expression:
	unary_expression
|	multiplicative_expression MULT_TK unary_expression
		{ 
		  $$ = build_binop (BINOP_LOOKUP ($2.token), 
				    $2.location, $1, $3);
		}
|	multiplicative_expression DIV_TK unary_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	multiplicative_expression REM_TK unary_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	multiplicative_expression MULT_TK error
		{yyerror ("Missing term"); RECOVER;}
|	multiplicative_expression DIV_TK error
		{yyerror ("Missing term"); RECOVER;}
|	multiplicative_expression REM_TK error
		{yyerror ("Missing term"); RECOVER;}
;

additive_expression:
	multiplicative_expression
|	additive_expression PLUS_TK multiplicative_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	additive_expression MINUS_TK multiplicative_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	additive_expression PLUS_TK error
		{yyerror ("Missing term"); RECOVER;}
|	additive_expression MINUS_TK error
		{yyerror ("Missing term"); RECOVER;}
;

shift_expression:
	additive_expression
|	shift_expression LS_TK additive_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	shift_expression SRS_TK additive_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	shift_expression ZRS_TK additive_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	shift_expression LS_TK error
		{yyerror ("Missing term"); RECOVER;}
|	shift_expression SRS_TK error
		{yyerror ("Missing term"); RECOVER;}
|	shift_expression ZRS_TK error
		{yyerror ("Missing term"); RECOVER;}
;

relational_expression:
	shift_expression
|	relational_expression LT_TK shift_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	relational_expression GT_TK shift_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	relational_expression LTE_TK shift_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	relational_expression GTE_TK shift_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	relational_expression INSTANCEOF_TK reference_type
|	relational_expression LT_TK error
		{yyerror ("Missing term"); RECOVER;}
|	relational_expression GT_TK error
		{yyerror ("Missing term"); RECOVER;}
|	relational_expression LTE_TK error
		{yyerror ("Missing term"); RECOVER;}
|	relational_expression GTE_TK error
		{yyerror ("Missing term"); RECOVER;}
|	relational_expression INSTANCEOF_TK error
		{yyerror ("Invalid reference type"); RECOVER;}
;

equality_expression:
	relational_expression
|	equality_expression EQ_TK relational_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	equality_expression NEQ_TK relational_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	equality_expression EQ_TK error
		{yyerror ("Missing term"); RECOVER;}
|	equality_expression NEQ_TK error
		{yyerror ("Missing term"); RECOVER;}
;

and_expression:
	equality_expression
|	and_expression AND_TK equality_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	and_expression AND_TK error
		{yyerror ("Missing term"); RECOVER;}
;

exclusive_or_expression:
	and_expression
|	exclusive_or_expression XOR_TK and_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	exclusive_or_expression XOR_TK error
		{yyerror ("Missing term"); RECOVER;}
;

inclusive_or_expression:
	exclusive_or_expression
|	inclusive_or_expression OR_TK exclusive_or_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	inclusive_or_expression OR_TK error
		{yyerror ("Missing term"); RECOVER;}
;

conditional_and_expression:
	inclusive_or_expression
|	conditional_and_expression BOOL_AND_TK inclusive_or_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	conditional_and_expression BOOL_AND_TK error
		{yyerror ("Missing term"); RECOVER;}
;

conditional_or_expression:
	conditional_and_expression
|	conditional_or_expression BOOL_OR_TK conditional_and_expression
		{
		  $$ = build_binop (BINOP_LOOKUP ($2.token), $2.location,
				    $1, $3); 
		}
|	conditional_or_expression BOOL_OR_TK error
		{yyerror ("Missing term"); RECOVER;}
;

conditional_expression:		/* Error handling here is weak */
	conditional_or_expression
|	conditional_or_expression REL_QM_TK expression REL_CL_TK conditional_expression
|	conditional_or_expression REL_QM_TK REL_CL_TK error
		{
		  YYERROR_NOW;
		  yyerror ("Missing term");
		  DRECOVER (1);
		}
|	conditional_or_expression REL_QM_TK error
		{yyerror ("Missing term"); DRECOVER (2);}
|	conditional_or_expression REL_QM_TK expression REL_CL_TK error
		{yyerror ("Missing term"); DRECOVER (3);}
;

assignment_expression:
	conditional_expression
|	assignment
;

assignment:
	left_hand_side assignment_operator assignment_expression
		{ $$ = build_assignment ($2.token, $2.location, $1, $3); }
|	left_hand_side assignment_operator error
		{
		  if (ctxp->prevent_ese != lineno)
		    yyerror ("Missing term");
		  DRECOVER (assign);
		}
;

left_hand_side:
	name
|	field_access
|	array_access
;

assignment_operator:
	ASSIGN_ANY_TK
|	ASSIGN_TK
;

expression:
	assignment_expression
;

constant_expression:
	expression
;

%%


#include "lex.c"

/* Flag for the error report routine to issue the error the first time
   it's called (overriding the default behavior which is to drop the
   first invocation and honor the second one, taking advantage of a
   richer context.  */
static int force_error = 0;

/* Create a new parser context and make it the current one. */

void
java_push_parser_context ()
{
  struct parser_ctxt *new = 
    (struct parser_ctxt *)malloc(sizeof (struct parser_ctxt));

  bzero (new, sizeof (struct parser_ctxt));
  new->next = ctxp;
  ctxp = new;
  if (ctxp->next)
    ctxp->incomplete_class = ctxp->next->incomplete_class;
}  

void
java_parser_context_save_global ()
{
  ctxp->finput = finput;
  ctxp->lineno = lineno;
  ctxp->current_class = current_class;
  ctxp->filename = input_filename;
  ctxp->current_function_decl = current_function_decl;
}

void
java_parser_context_restore_global ()
{
  finput = ctxp->finput;
  lineno = ctxp->lineno;
  current_class = ctxp->current_class;
  input_filename = ctxp->filename;
  current_function_decl = ctxp->current_function_decl;
}

void 
java_pop_parser_context ()
{
  tree current;
  struct parser_ctxt *toFree = ctxp;
  struct parser_ctxt *next = ctxp->next;

  if (next)
    {
      next->incomplete_class = ctxp->incomplete_class;
      lineno = ctxp->lineno;
      finput = ctxp->finput;
      current_class = ctxp->current_class;
    }

  /* Set the single import class file flag to 0 for the current list
     of imported things */
  for (current = ctxp->import_list; current; current = TREE_CHAIN (current))
    IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (TREE_PURPOSE (current)) = 0;

  /* And restore those of the previous context */
  if (ctxp = next)
    for (current = ctxp->import_list; current; current = TREE_CHAIN (current))
      IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (TREE_PURPOSE (current)) = 1;

  free (toFree);
}

static int do_warning = 0;

void
yyerror (msg)
     char *msg;
{
  static java_lc elc;
  static int  prev_lineno;
  static char *prev_msg;

  int i, save_lineno;
  char *remainder, *code_from_source;
  extern struct obstack temporary_obstack;
  
  if (!force_error && prev_lineno == lineno)
    return;

  /* Save current error location but report latter, when the context is
     richer.  */
  if (ctxp->java_error_flag == 0)
    {
      ctxp->java_error_flag = 1;
      elc = ctxp->elc;
      /* Do something to use the previous line if we're reaching the
	 end of the file... */
#ifdef VERBOSE_SKELETON
      printf ("* Error detected (%s)\n", (msg ? msg : "(null)"));
#endif
      return;
    }

  /* Ignore duplicate message on the same line. BTW, this is dubious. FIXME */
  if (!force_error && msg == prev_msg && prev_lineno == elc.line)
    return;

  ctxp->java_error_flag = 0;
  if (do_warning)
    java_warning_count++;
  else
    java_error_count++;
  
  if (elc.col == 0 && msg[1] == ';')
    {
      elc.col  = ctxp->p_line->char_col-1;
      elc.line = ctxp->p_line->lineno;
    }

  save_lineno = lineno;
  prev_lineno = lineno = elc.line;
  prev_msg = msg;

  code_from_source = java_get_line_col (ctxp->filename, elc.line, elc.col);
  obstack_grow0 (&temporary_obstack, 
		 code_from_source, strlen (code_from_source));
  remainder = obstack_finish (&temporary_obstack);
  if (do_warning)
    warning ("%s.\n%s", msg, remainder);
  else
    error ("%s.\n%s", msg, remainder);

  /* This allow us to cheaply avoid an extra 'Invalid expression
     statement' error report when errors have been already reported on
     the same line. This occurs when we report an error but don't have
     a synchronization point other than ';', which
     expression_statement is the only one to take care of.  */
  ctxp->prevent_ese = lineno = save_lineno;
}

static void
parse_error (msg)
     char *msg;
{
  java_error (NULL);
  java_error (msg);
}

/* Issue an error message at a current source line CL */

static void
parse_error_context VPROTO ((tree cl, char *msg, ...))
{
#ifndef __STDC__
  tree cl;
  char *msg;
#endif
  char buffer [4096];
  va_list ap;

  VA_START (ap, msg);
#ifndef __STDC__
  cl = va_arg (ap, tree);
  msg = va_arg (ap, char *);
#endif
  vsprintf (buffer, msg, ap);

  force_error = 1;
  ctxp->elc.line = EXPR_WFL_LINENO (cl);
  ctxp->elc.col  = (EXPR_WFL_COLNO (cl) == 0xfff ? -1 : EXPR_WFL_COLNO (cl));

  parse_error (buffer);
  force_error = 0;
}

/* Issue a warning at a current source line CL */

static void
parse_warning_context VPROTO ((tree cl, char *msg, ...))
{
#ifndef __STDC__
  tree cl;
  char *msg;
#endif
  char buffer [4096];
  va_list ap;

  VA_START (ap, msg);
#ifndef __STDC__
  cl = va_arg (ap, tree);
  msg = va_arg (ap, char *);
#endif
  vsprintf (buffer, msg, ap);

  force_error = do_warning = 1;
  ctxp->elc.line = EXPR_WFL_LINENO (cl);
  ctxp->elc.col  = (EXPR_WFL_COLNO (cl) == 0xfff ? -1 : EXPR_WFL_COLNO (cl));

  parse_error (buffer);
  do_warning = force_error = 0;
}

void
java_report_errors ()
{
  if (java_error_count)
    fprintf (stderr, "%d error%s", 
	     java_error_count, (java_error_count == 1 ? "" : "s"));
  if (java_warning_count)
    fprintf (stderr, "%s%d warning%s", (java_error_count ? ", " : ""),
	     java_warning_count, (java_warning_count == 1 ? "" : "s"));
  if (java_error_count || java_warning_count)
    putc ('\n', stderr);
}

static char *
java_accstring_lookup (flags)
     int flags;
{
  static char buffer [80];
#define COPY_RETURN(S) {strcpy (buffer, S); return buffer;}

  /* Access modifier looked-up first for easier report on forbidden
     access. */
  if (flags & ACC_PUBLIC) COPY_RETURN ("public");
  if (flags & ACC_PRIVATE) COPY_RETURN ("private");
  if (flags & ACC_PROTECTED) COPY_RETURN ("protected");
  if (flags & ACC_STATIC) COPY_RETURN ("static");
  if (flags & ACC_FINAL) COPY_RETURN ("final");
  if (flags & ACC_SYNCHRONIZED) COPY_RETURN ("synchronized");
  if (flags & ACC_VOLATILE) COPY_RETURN ("volatile");
  if (flags & ACC_TRANSIENT) COPY_RETURN ("transient");
  if (flags & ACC_NATIVE) COPY_RETURN ("native");
  if (flags & ACC_INTERFACE) COPY_RETURN ("interface");
  if (flags & ACC_ABSTRACT) COPY_RETURN ("abstract");

  buffer [0] = '\0';
  return buffer;
#undef COPY_RETURN
}

static void
redefinition_error (context, id, decl, cl)
     char *context;
     tree id, decl, cl;
{
  parse_error_context (cl, "%s `%s' already defined in %s:%d", 
		       context, IDENTIFIER_POINTER (id), 
		       DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
  /* Here we should point out where its redefined. It's a unicode. FIXME */
}

/* Build something that the type identifier resolver will identify as
   being an array to an unresolved type. TYPE_WFL is a WFL on a
   identifier. */

static tree
build_unresolved_array_type (type_or_wfl)
     tree type_or_wfl;
{
  char *ptr;

  /* TYPE_OR_WFL might be an array on a primitive type. In this case,
     just create a array type */
  if (TREE_CODE (type_or_wfl) == RECORD_TYPE)
    {
      tree type = build_java_array_type (type_or_wfl, -1);
      CLASS_LOADED_P (type) = CLASS_LOADED_P (type_or_wfl);
      return type;
    }

  obstack_1grow (&temporary_obstack, '[');
  obstack_grow0 (&temporary_obstack,
		 IDENTIFIER_POINTER (EXPR_WFL_NODE (type_or_wfl)),
		 IDENTIFIER_LENGTH (EXPR_WFL_NODE (type_or_wfl)));
  ptr = obstack_finish (&temporary_obstack);
  return build_expr_wfl (get_identifier (ptr),
			 EXPR_WFL_FILENAME (type_or_wfl),
			 EXPR_WFL_LINENO (type_or_wfl),
			 EXPR_WFL_COLNO (type_or_wfl));
}

/* Check modifiers. If one doesn't fit, retrieve it in its declaration line
  and point it out.  */

static void
check_modifiers (message, value, mask)
     char *message;
     int value;
     int mask;
{
  /* Should point out the one that don't fit. ASCII/unicode,
     going backward. FIXME */
  if (value & ~mask)
    {
      int i, remainder = value & ~mask;
      for (i = 0; i <= 10; i++)
        if ((1 << i) & remainder)
	  parse_error_context (ctxp->modifier_ctx [i], message, 
			       java_accstring_lookup (1 << i));
    }
}

static void
parser_add_interface (class_decl, interface_decl, wfl)
     tree class_decl, interface_decl, wfl;
{
  if (maybe_add_interface (TREE_TYPE (class_decl), TREE_TYPE (interface_decl)))
    parse_error_context (wfl, "Interface `%s' repeated",
			 IDENTIFIER_POINTER (DECL_NAME (interface_decl)));
}

/* Bulk of common class/interface checks. Return 1 if an error was
   encountered. TAG is 0 for a class, 1 for an interface.  */

static int
check_class_interface_creation (is_interface, flags, raw_name, qualified_name, decl, cl)
     int is_interface, flags;
     tree raw_name, qualified_name, decl, cl;
{
  tree node;

  if (!quiet_flag)
    fprintf (stderr, " %s %s", (is_interface ? "interface" : "class"), 
	     IDENTIFIER_POINTER (qualified_name));

  /* Scope of an interface/class type name:
       - Can't be imported by a single type import
       - Can't already exists in the package */
  if (IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (raw_name)
      && (node = find_name_in_single_imports (raw_name)))
    {
      parse_error_context 
	(cl, "%s name `%s' clashes with imported type `%s'",
	 (is_interface ? "Interface" : "Class"),
	 IDENTIFIER_POINTER (raw_name), IDENTIFIER_POINTER (node));
      return 1;
    }
  if (decl && CLASS_COMPLETE_P (decl))
    {
      redefinition_error ((is_interface ? "Interface" : "Class"), 
			  qualified_name, decl, cl);
      return 1;
    }

  /* If public, file name should match class/interface name */
  if (flags & ACC_PUBLIC)
    {
      char *f;

      /* Contains OS dependent assumption on path separator. FIXME */
      for (f = &input_filename [strlen (input_filename)]; 
	   f != input_filename && f[0] != '/'; f--);
      if (f[0] == '/')
	f++;
      if (strncmp (IDENTIFIER_POINTER (raw_name), 
		   f , IDENTIFIER_LENGTH (raw_name)) ||
	  f [IDENTIFIER_LENGTH (raw_name)] != '.')
	parse_error_context (cl, "Public %s `%s' must be defined in a file "
			     "called `%s.java'", 
			     (is_interface ? "interface" : "class"),
			     IDENTIFIER_POINTER (qualified_name),
			     IDENTIFIER_POINTER (raw_name));
    }

  check_modifiers ((is_interface ? 
		    "Illegal modifier `%s' for interface declaration" :
		    "Illegal modifier `%s' for class declaration"), flags,
		   (is_interface ? INTERFACE_MODIFIERS : CLASS_MODIFIERS));
  return 0;
}

/* If DECL is NULL, create and push a new DECL, record the current
   line CL and do other maintenance things.  */

static tree
maybe_create_class_interface_decl (decl, qualified_name, cl)
     tree decl, qualified_name, cl;
{
  if (decl)
    DECL_ARTIFICIAL (decl) = 1; /* FIXME */
  else
    decl = push_class (make_class (), qualified_name);
  
  /* Take care of the file and line business */
  DECL_SOURCE_FILE (decl) = EXPR_WFL_FILENAME (cl);
  DECL_SOURCE_LINE (decl) = EXPR_WFL_LINENO (cl);
  CLASS_FROM_SOURCE_P (TREE_TYPE (decl)) = 1;

  ctxp->current_parsed_class = decl;
  
  /* Link the declaration to the already seen ones */
  TREE_CHAIN (decl) = ctxp->class_list;
  ctxp->class_list = decl;
  /* Install a new dependency list element */
  create_jdep_list (ctxp);

  SOURCE_FRONTEND_DEBUG (("Defining class/interface %s", 
			  IDENTIFIER_POINTER (qualified_name)));
  return decl;
}

static void
add_superinterfaces (decl, interface_list)
     tree decl, interface_list;
{
  tree node;
  /* Superinterface(s): if present and defined, parser_check_super_interface ()
     takes care of ensuring that:
       - This is an accessible interface type,
       - Circularity detection.
   parser_add_interface is then called. If present but not defined,
   the check operation is delayed until the super interface gets
   defined.  */
  for (node = interface_list; node; node = TREE_CHAIN (node))
    {
      tree current = TREE_PURPOSE (node), interface_decl;
      if ((interface_decl = IDENTIFIER_CLASS_VALUE (EXPR_WFL_NODE (current))))
	{
	  if (!parser_check_super_interface (interface_decl, decl, current))
	    parser_add_interface (decl, interface_decl, current);
	}
      else
	register_incomplete_type (JDEP_INTERFACE,
				  current, decl, NULL_TREE);
    }
}

/* Create an interface in pass1 and return its decl. Return the
   interface's decl in pass 2.  */

static tree
create_interface (flags, id, super)
     int flags;
     tree id, super;
{
  int chk;
  tree raw_name = EXPR_WFL_NODE (id);
  tree q_name = parser_qualified_classname (id);
  tree decl = IDENTIFIER_CLASS_VALUE (q_name);

  EXPR_WFL_NODE (id) = q_name;	/* Keep source location, even if refined. */

  /* Basic checks: scope, redefinition, modifiers */ 
  if (check_class_interface_creation (1, flags, raw_name, q_name, decl, id))
    return NULL_TREE;

  /* Interface modifiers check
       - public/abstract allowed (already done at that point)
       - abstract is obsolete (comes first, it's a warning, or should be)
       - Can't use twice the same (checked in the modifier rule) */
  if (flags & ACC_ABSTRACT)
    parse_warning_context 
      (MODIFIER_WFL (ABSTRACT_TK),
       "Obsolete use of `abstract' modifier. Interface `%s' is implicitely "
       "abstract", IDENTIFIER_POINTER (raw_name));
  if (flags & ACC_PUBLIC && flags & ACC_ABSTRACT)
    parse_error_context 
      (MODIFIER_WFL (ABSTRACT_TK),
       "Can't specify both `public' and `abstract' modifiers in the "
       "definition of interface `%s'", IDENTIFIER_POINTER (raw_name));

  /* Create a new decl if DECL is NULL, otherwise fix it */
  decl = maybe_create_class_interface_decl (decl, q_name, id);

  /* Set super info and mark the class a complete */
  set_super_info (ACC_ABSTRACT | ACC_INTERFACE | flags, TREE_TYPE (decl), 
		  object_type_node, ctxp->interface_number);
  ctxp->interface_number = 0;
  CLASS_COMPLETE_P (decl) = 1;
  add_superinterfaces (decl, super);

  return decl;
}

/* Create an class in pass1 and return its decl. Return class
   interface's decl in pass 2.  */

static tree
create_class (flags, id, super, interfaces)
     int flags;
     tree id, super, interfaces;
{
  int chk;
  tree raw_name = EXPR_WFL_NODE (id);
  tree class_id, decl;
  tree super_decl = NULL, super_decl_type;

  class_id = parser_qualified_classname (id);
  decl = IDENTIFIER_CLASS_VALUE (class_id);
  EXPR_WFL_NODE (id) = class_id;

  /* Basic check: scope, redefinition, modifiers */
  if (check_class_interface_creation (0, flags, raw_name, class_id, decl, id))
    return NULL_TREE;

  /* Class modifier check: 
       - Allowed modifier (already done at that point)
       - abstract AND final forbidden 
       - Public classes defined in the correct file */
  if ((flags & ACC_ABSTRACT) && (flags & ACC_FINAL))
    parse_error_context (id, "Class `%s' can't be declared both abstract "
			 "and final", IDENTIFIER_POINTER (raw_name));

  /* Create a new decl if DECL is NULL, otherwise fix it */
  decl = maybe_create_class_interface_decl (decl, class_id, id);

  /* If SUPER exists, use it, otherwise use Object */
  if (super)
    {
      /* Can't extend java.lang.Object */
      if (TREE_TYPE (IDENTIFIER_CLASS_VALUE (class_id)) == object_type_node)
	{
	  parse_error_context (id, "Can't extend `java.lang.Object'");
	  return NULL_TREE;
	}

      /* The class is known and exists if there is a decl. Otherwise,
         postpone the operation and do it later. */
      super_decl = IDENTIFIER_CLASS_VALUE (EXPR_WFL_NODE (super));
      if (super_decl)
	{
	  parser_check_super (super_decl, decl, id);
	  super_decl_type = TREE_TYPE (super_decl);
	}
      else
	super_decl_type = 
	  register_incomplete_type (JDEP_SUPER, super, decl, NULL_TREE);
    }
  else if (TREE_TYPE (decl) != object_type_node)
    super_decl_type = object_type_node;
  /* We're defining java.lang.Object */
  else
    super_decl_type = NULL_TREE;

  /* Set super info and mark the class a complete */
  set_super_info (flags, TREE_TYPE (decl), super_decl_type, 
		  ctxp->interface_number);
  ctxp->interface_number = 0;
  CLASS_COMPLETE_P (decl) = 1;
  add_superinterfaces (decl, interfaces);

  return decl;
}

/* Can't use lookup_field () since we don't want to load the class and
   can't set the CLASS_LOADED_P flag */

static tree
find_field (class, name)
     tree class;
     tree name;
{
  tree decl;
  for (decl = TYPE_FIELDS (class); decl; decl = TREE_CHAIN (decl))
    {
      if (DECL_NAME (decl) == name)
	return decl;
    }
  return NULL_TREE;
}

/* Wrap around lookup_field that doesn't potentially upset the value
   of CLASS */

static tree
lookup_field_wrapper (class, name)
     tree class, name;
{
  tree type = class;
  return lookup_field (&type, name);
}

/* Find duplicate field within the same class declarations and report
   the error */

static int
duplicate_declaration_error (class, new_field_name, new_type, cl)
     tree class, new_field_name, new_type, cl;
{
  /* This might be modified to work with method decl as well */
  tree decl = find_field (TREE_TYPE (ctxp->current_parsed_class), 
			  new_field_name);
  if (decl)
    {
      char *t1 = strdup ((char *)lang_printable_name (new_type, 1));
      char *t2 = 
	strdup ((TREE_CODE (TREE_TYPE (decl)) == TREE_LIST ?
		 IDENTIFIER_POINTER (TYPE_NAME 
				     (TREE_PURPOSE (TREE_TYPE (decl)))) :
		 (char *)lang_printable_name (TREE_TYPE (decl), 1)));
      parse_error_context 
	(cl , "Duplicate variable declaration: `%s %s' was `%s %s' (%s:%d)", 
	 t1, IDENTIFIER_POINTER (new_field_name),
	 t2, IDENTIFIER_POINTER (DECL_NAME (decl)),
	 DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
      free (t1);
      free (t2);
      return 0;
    }
  return 1;
}

/* Field registration routine. If TYPE doesn't exist, field
   declarations are linked to the undefined TYPE dependency list, to
   be later resolved in java_complete_class () */

static void
register_fields (flags, type, variable_list)
     int flags;
     tree type, variable_list;
{
  tree current, type_decl, returned_type;
  tree class_type = TREE_TYPE (ctxp->current_parsed_class);
  int saved_lineno = lineno;
  int must_chain = 0;
  tree wfl = NULL_TREE;

  /* If we're adding fields to interfaces, those fields are public,
     static, final */
  if (CLASS_INTERFACE (TYPE_NAME (class_type)))
    {
      OBSOLETE_MODIFIER_WARNING (MODIFIER_WFL (PUBLIC_TK),
				 flags, ACC_PUBLIC, 
				 "%s", "interface field(s)");
      OBSOLETE_MODIFIER_WARNING (MODIFIER_WFL (STATIC_TK),
				 flags, ACC_STATIC, 
				 "%s", "interface field(s)");
      OBSOLETE_MODIFIER_WARNING (MODIFIER_WFL (FINAL_TK),
				 flags, ACC_FINAL, "%s", "interface field(s)");
      check_modifiers ("Illegal interface member modifier `%s'", flags,
		       INTERFACE_FIELD_MODIFIERS);
      flags |= (ACC_PUBLIC | ACC_STATIC | ACC_FINAL);
    }

  if (unresolved_type_p (type, &returned_type))
    {
      if (returned_type)
	type = returned_type;
      else
	{
	  wfl = type;
	  type = obtain_incomplete_type (type);
	  must_chain = 1;
	}
    }

  for (current = variable_list; current; current = TREE_CHAIN (current))
    {
      tree cl = TREE_PURPOSE (current);
      tree init = TREE_VALUE (current);
      tree current_name = EXPR_WFL_NODE (cl);

      if (duplicate_declaration_error (class_type, current_name, type, cl))
        {
	  tree field_decl;
	  lineno = EXPR_WFL_LINENO (cl);
	  field_decl = add_field (class_type, current_name, type, flags);

	  /* Check if we must chain. */
	  if (must_chain)
	    register_incomplete_type (JDEP_FIELD, wfl, field_decl, type);
	  
	  /* Default value of a static field is 0 and it is considered
             initialized. */
	  if (flags & ACC_STATIC)
	    INITIALIZED_P (field_decl) = 1;

	  /* If we have an initialization value tied to the field */
	  if (init)
	    {
	      /* The field is declared static */
	      if (flags & ACC_STATIC)
		{
		  /* FIXME */
		  if (flags & ACC_FINAL)
		    ;		
		  /* Otherwise, the field should be initialized in
		     <clinit>. This field is remembered so we can
		     generate <clinit> later. */
		  else
		    {
		      INITIALIZED_P (field_decl) = 1;
		      TREE_CHAIN (init) = ctxp->static_initialized;
		      ctxp->static_initialized = init;
		    }
		}
	      /* A non-static field declared with an immediate
		 initialization is to be initialized in <init>, if
		 any.  This field is remembered to be processed at the
		 time of the generation of <init>. */
	      else
		{
		  TREE_CHAIN (init) = ctxp->non_static_initialized;
		  ctxp->non_static_initialized = init;
		}
	    }
	}
    }
  lineno = saved_lineno;
}

/* Check whether it is necessary to generate a <clinit> for the class
   we just parsed. */

static void
maybe_generate_clinit ()
{
  int saved_lineno;
  tree meth, mdecl, c;
  tree cclass, class_wfl;

  if (!ctxp->static_initialized || java_error_count)
    return;

  cclass = TREE_TYPE (ctxp->current_parsed_class);
  class_wfl = build_expr_wfl (DECL_NAME (TYPE_NAME (cclass)),
			      input_filename, 0, 0);
  
  saved_lineno = lineno;
  lineno = 0;
  meth = make_node (FUNCTION_TYPE);
  TREE_TYPE (meth) = void_type_node;
  TYPE_ARG_TYPES (meth) = NULL_TREE;
  mdecl = add_method (cclass, ACC_STATIC, clinit_identifier_node,
		     build_java_signature (meth));
  lineno = saved_lineno;

  DECL_SOURCE_LINE (mdecl) = 1;
  DECL_SOURCE_LINE_MERGE (mdecl, 1);
  source_start_java_method (mdecl);
  enter_block ();

  /* Keep initialization in order to enforce 8.5 */
  ctxp->static_initialized = nreverse (ctxp->static_initialized);

  /* We process the list of assignment we produced as the result of
     the declaration of initialized static field and add them as
     statement to the <clinit> method. */
  for (c = ctxp->static_initialized; c; c = TREE_CHAIN (c))
    {
      /* We build the assignment expression that will initialize the
	 field to its value. There are strict rules on static
	 initializers (8.5). FIXME */
      java_method_add_stmt (mdecl, c);
    }

  BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (mdecl)) = exit_block ();
  exit_block ();
  ctxp->static_initialized = NULL_TREE;
}

/* Shared accros method_declarator and method_header to remember the
   patch stage that was reached during the declaration of the method.
   A method DECL is built differently is there is no patch
   (JDEP_NO_PATCH) or a patch (JDEP_METHOD or JDEP_METHOD_RETURN)
   pending on the currently defined method.  */

static int patch_stage;

/* Check the method declaration and add the method to its current
   class.  If the argument list is known to contain incomplete types,
   the method is partially added and the registration will be resume
   once the method arguments resolved  */

static tree
method_header (flags, type, mdecl, throws)
     int flags;
     tree type, mdecl, throws;
{
  tree meth = TREE_VALUE (mdecl);
  tree id = TREE_PURPOSE (mdecl);
  tree this_class = TREE_TYPE (ctxp->current_parsed_class);
  tree handle_class = CLASS_TO_HANDLE_TYPE (this_class);
  tree meth_name, returned_type;
  int saved_lineno;
  
  check_modifiers_consistency (flags);
  
  /* There are some forbidden modifiers for an abstract method and its
     class must be abstract as well.  */
  if (flags & ACC_ABSTRACT)
    {
      ABSTRACT_CHECK (flags, ACC_PRIVATE, id, "Private");
      ABSTRACT_CHECK (flags, ACC_STATIC, id, "Static");
      ABSTRACT_CHECK (flags, ACC_FINAL, id, "Final");
      ABSTRACT_CHECK (flags, ACC_NATIVE, id, "Native");
      ABSTRACT_CHECK (flags, ACC_SYNCHRONIZED,id, "Synchronized");
      if (!CLASS_ABSTRACT (TYPE_NAME (this_class)))
	parse_error_context 
	  (id, "Class `%s' must be declared abstract to define abstract "
	   "method `%s'", 
	   IDENTIFIER_POINTER (DECL_NAME (ctxp->current_parsed_class)),
	   IDENTIFIER_POINTER (EXPR_WFL_NODE (id)));
    }


  /* Method declared within the scope of an interface are implicitly
     abstract and public. Conflicts with other erroneously provided
     modifiers are check right after. */

  if (CLASS_INTERFACE (TYPE_NAME (this_class)))
    {
      /* If FLAGS isn't set because of a modifier, turn the
	 corresponding modifier WFL to NULL so we issue a warning on
	 the obsolete use of the modifier */
      if (!(flags & ACC_PUBLIC))
        MODIFIER_WFL (PUBLIC_TK) = NULL;
      if (!(flags & ACC_ABSTRACT))
        MODIFIER_WFL (ABSTRACT_TK) = NULL;
      flags |= ACC_PUBLIC;
      flags |= ACC_ABSTRACT;
    }

  /* Modifiers context reset moved up, so abstract method declaration
     modifiers can be later checked.  */

  meth_name = EXPR_WFL_NODE (id);

  if (unresolved_type_p (type, &returned_type))
    {
      if (returned_type)
	TREE_TYPE (meth) = returned_type;
      else 
	{
	  patch_stage = JDEP_METHOD_RETURN;
	  TREE_TYPE (meth) = 
	    register_incomplete_type (patch_stage, type, id, NULL_TREE);
	}
    }
  else
    TREE_TYPE (meth) = type;

  saved_lineno = lineno;
  /* When defining an abstract or interface method, the curly
     bracket at level 1 doesn't exist because there is no function
     body */
  lineno = (ctxp->first_ccb_indent1 ? ctxp->first_ccb_indent1 : 
	    EXPR_WFL_LINENO (id));

  if (patch_stage)		/* includes ret type and/or all args */
    {
      jdep *jdep;
      meth = add_method_1 (this_class, flags, meth_name, meth);
      /* Patch for the return type */
      if (patch_stage == JDEP_METHOD_RETURN)
	{
	  jdep = CLASSD_LAST (ctxp->classd_list);
	  JDEP_GET_PATCH (jdep) = &TREE_TYPE (TREE_TYPE (meth));
	}
      /* This is the stop JDEP. METH allows the function's signature
	 to be computed. */
      register_incomplete_type (JDEP_METHOD_END, NULL_TREE, meth, NULL_TREE);
    }
  else
    {
      tree signature = build_java_signature (meth);
      tree arg, orig_arg;
      /* Save original argument list, including argument's names */
      orig_arg = TYPE_ARG_TYPES (meth);
      /* Add the method to its class */
      meth = add_method (this_class, flags, meth_name, signature);
      /* Fix the method argument list so we have the argument name
         information */
      arg = TYPE_ARG_TYPES (TREE_TYPE (meth));
      if (TREE_CODE (TREE_TYPE (meth)) == METHOD_TYPE)
	{
	  TREE_PURPOSE (arg) = this_identifier_node;
	  arg = TREE_CHAIN (arg);
	}
      while (orig_arg)
        {
	  TREE_PURPOSE (arg) = TREE_PURPOSE (orig_arg);
	  orig_arg = TREE_CHAIN (orig_arg);
	  arg = TREE_CHAIN (arg);
	}
    }
  DECL_MAX_LOCALS (meth) = ctxp->formal_parameter_number+1;
  lineno = saved_lineno;
  /* We set the DECL_NAME to ID so we can track the location where
     the function was declared. This allow us to report
     redefinition error accurately. When method are verified,
     DECL_NAME is reinstalled properly (using the content of the
     WFL node ID) (see check_method_redefinition). We don't do that
     when Object is being defined. */
  if (TREE_TYPE (ctxp->current_parsed_class) != object_type_node)
    DECL_NAME (meth) = id;
  return meth;
}

/* Check modifiers that can be declared but exclusively */

static void
check_modifiers_consistency (flags)
     int flags;
{
  int acc_count = 0;
  tree cl = NULL_TREE;

  THIS_MODIFIER_ONLY (flags, ACC_PUBLIC, 0, acc_count, cl);
  THIS_MODIFIER_ONLY (flags, ACC_PRIVATE, 1, acc_count, cl);
  THIS_MODIFIER_ONLY (flags, ACC_PROTECTED, 2, acc_count, cl);
  if (acc_count > 1)
    parse_error_context
      (cl, "Inconsistent member declaration. At most one of `public', "
       "`private', or `protected' may be specified");
}

/* Check the methode header METH for abstract specifics features */

static void
check_abstract_method_header (meth)
     tree meth;
{
  int flags = get_access_flags_from_decl (meth);
  /* DECL_NAME might still be a WFL node */
  tree name = (TREE_CODE (DECL_NAME (meth)) == EXPR_WITH_FILE_LOCATION ?
	       EXPR_WFL_NODE (DECL_NAME (meth)) : DECL_NAME (meth));

  OBSOLETE_MODIFIER_WARNING (MODIFIER_WFL (ABSTRACT_TK), flags,
			     ACC_ABSTRACT, "abstract method `%s'",
			     IDENTIFIER_POINTER (name));
  OBSOLETE_MODIFIER_WARNING (MODIFIER_WFL (PUBLIC_TK), flags, 
			     ACC_PUBLIC, "abstract method `%s'",
			     IDENTIFIER_POINTER (name));

  check_modifiers ("Illegal modifier `%s' for interface method",
		  flags, INTERFACE_METHOD_MODIFIERS);
}

/* Create a FUNCTION_TYPE node and start augmenting it with the
   declared function arguments. Arguments type that can't be resolved
   are left as they are, but the returned node is marked as containing
   incomplete types.  */

static tree
method_declarator (id, list)
     tree id, list;
{
  tree arg_types = NULL_TREE, current, node;
  tree meth = make_node (FUNCTION_TYPE);
  jdep *jdep;
  int incomplete = 0;

  patch_stage = JDEP_NO_PATCH;
  
  for (current = list; current; current = TREE_CHAIN (current))
    {
      tree wfl_name = TREE_PURPOSE (current);
      tree type = TREE_VALUE (current);
      tree name = EXPR_WFL_NODE (wfl_name);
      tree patchable_type = NULL_TREE, already;
      tree arg_node, returned_type;

      /* Check redefinition */
      for (already = arg_types; already; already = TREE_CHAIN (already))
	if (TREE_PURPOSE (already) == name)
	  {
	    parse_error_context 
	      (wfl_name, "Variable `%s' is used more than once in the "
	       "argument list of method `%s'", IDENTIFIER_POINTER (name),
	       IDENTIFIER_POINTER (EXPR_WFL_NODE (id)));
	    break;
	  }

      /* If we've an incomplete argument type, we know there is a location
	 to patch when the type get resolved, later.  */
      jdep = NULL;
      if (unresolved_type_p (type, &returned_type))
	{
	  if (returned_type)
	    type = returned_type;
	  else
	    {
	      patch_stage = JDEP_METHOD;
	      type = register_incomplete_type (patch_stage, type, 
					       wfl_name, NULL_TREE);
	      jdep = CLASSD_LAST (ctxp->classd_list);
	      JDEP_MISC (jdep) = id;
	    }
	}
      /* The argument node: a name and a (possibly) incomplete type */
      arg_node = build_tree_list (name, type);
      if (jdep)
	JDEP_GET_PATCH (jdep) = &TREE_VALUE (arg_node);
      TREE_CHAIN (arg_node) = arg_types;
      arg_types = arg_node;
    }
  TYPE_ARG_TYPES (meth) = nreverse (arg_types);
  node = build_tree_list (id, meth);
  return node;
}

static int
unresolved_type_p (wfl, returned)
     tree wfl;
     tree *returned;
     
{
  if (TREE_CODE (wfl) == EXPR_WITH_FILE_LOCATION)
    {
      tree decl = IDENTIFIER_CLASS_VALUE (EXPR_WFL_NODE (wfl));
      if (returned)
	*returned = (decl ? TREE_TYPE (decl) : NULL_TREE);
      return 1;
    }
  if (returned)
    *returned = wfl;
  return 0;
}

/* From NAME, build a qualified identifier node using the
   qualification from the current package definition. */

static tree
parser_qualified_classname (name)
     tree name;
{
  if (ctxp->package)
    return merge_qualified_name (ctxp->package, EXPR_WFL_NODE (name));
  else 
    return EXPR_WFL_NODE (name);
}

/* Called once the type a interface extends is resolved. Returns 0 if
   everything is OK.  */

static int
parser_check_super_interface (super_decl, this_decl, this_wfl)
     tree super_decl, this_decl, this_wfl;
{
  tree super_type = TREE_TYPE (super_decl);

  /* Has to be an interface */
  if (!CLASS_INTERFACE (TYPE_NAME (TREE_TYPE (super_decl))))
    {
      parse_error_context 
	(this_wfl, "Can't use %s `%s' to implement/extend %s `%s'",
	 (TYPE_ARRAY_P (super_type) ? "array" : "class"),
	 IDENTIFIER_POINTER (DECL_NAME (super_decl)),
	 (CLASS_INTERFACE (TYPE_NAME (TREE_TYPE (this_decl))) ? 
	  "interface" : "class"),
	 IDENTIFIER_POINTER (DECL_NAME (this_decl)));
      return 1;
    }

  /* Check scope: same package OK, other package: OK if public */
  if (check_pkg_class_access (DECL_NAME (super_decl), lookup_cl (this_decl)))
    return 1;

  SOURCE_FRONTEND_DEBUG (("Completing interface %s with %s",
			  IDENTIFIER_POINTER (DECL_NAME (this_decl)),
			  IDENTIFIER_POINTER (DECL_NAME (super_decl))));
  return 0;
}

/* Makes sure that SUPER_DECL is suitable to extend THIS_DECL. Returns
   0 if everthing is OK.  */

static int
parser_check_super (super_decl, this_decl, wfl)
     tree super_decl, this_decl, wfl;
{
  tree this_type  = TREE_TYPE (this_decl);
  tree super_type = TREE_TYPE (super_decl);

  /* SUPER should be a CLASS (neither an array nor an interface) */
  if (TYPE_ARRAY_P (super_type) || CLASS_INTERFACE (TYPE_NAME (super_type)))
    {
      parse_error_context 
	(wfl, "Class `%s' can't subclass %s `%s'",
	 IDENTIFIER_POINTER (DECL_NAME (this_decl)),
	 (CLASS_INTERFACE (TYPE_NAME (super_type)) ? "interface" : "array"),
	 IDENTIFIER_POINTER (DECL_NAME (super_decl)));
      return 1;
    }

  if (CLASS_FINAL (TYPE_NAME (super_type)))
    {
      parse_error_context (wfl, "Can't subclass final classes: %s",
			   IDENTIFIER_POINTER (DECL_NAME (super_decl)));
      return 1;
    }

  /* Check scope: same package OK, other package: OK if public */
  if (check_pkg_class_access (DECL_NAME (super_decl), wfl))
    return 1;
  
  SOURCE_FRONTEND_DEBUG (("Completing class %s with %s",
			  IDENTIFIER_POINTER (DECL_NAME (this_decl)),
			  IDENTIFIER_POINTER (DECL_NAME (super_decl))));
  return 0;
}

/* Create a new dependency list and link it (in a LIFO manner) to the
   CTXP list of type dependency list.  */

static void
create_jdep_list (ctxp)
     struct parser_ctxt *ctxp;
{
  jdeplist *new = malloc (sizeof (jdeplist));	
  
  if (!new)
    fatal ("Can't alloc jdeplist - create_jdep_list");
    
  new->first = new->last = NULL;
  new->next = ctxp->classd_list;
  ctxp->classd_list = new;
}

static jdeplist *
reverse_jdep_list (ctxp)
     struct parser_ctxt *ctxp;
{
  register jdeplist *prev = NULL, *current, *next;
  for (current = ctxp->classd_list; current; current = next)
    {
      next = current->next;
      current->next = prev;
      prev = current;
    }
  return prev;
}

/* Create a fake pointer based on the ID stored in the WFL */

static tree
obtain_incomplete_type (wfl)
     tree wfl;
{
  tree ptr;
  tree name = EXPR_WFL_NODE (wfl);

  for (ptr = ctxp->incomplete_class; ptr; ptr = TREE_CHAIN (ptr))
    if (TYPE_NAME (TREE_PURPOSE (ptr)) == name)
      break;

  if (!ptr)
    {
      tree core;
      push_obstacks (&permanent_obstack, &permanent_obstack);
      BUILD_PTR_FROM_NAME (core, name);
      ptr = build_tree_list (core, NULL_TREE);
      pop_obstacks ();
      TREE_CHAIN (ptr) = ctxp->incomplete_class;
      ctxp->incomplete_class = ptr;
    }

  return ptr;
}

/* Register a incomplete type whose name is WFL. Reuse PTR if PTR is
   non NULL instead of computing a new fake type based on WFL. The new
   dependency is inserted in the current type dependency list, in FIFO
   manner.  */

static tree
register_incomplete_type (kind, wfl, decl, ptr)
     int kind;
     tree wfl, decl, ptr;
{
  jdep *new = malloc (sizeof (jdep));

  if (!new)
    fatal ("Can't allocate new jdep - register_incomplete_type");
  if (!ptr && kind != JDEP_METHOD_END) /* JDEP_METHOD_END is a mere marker */
    ptr = obtain_incomplete_type (wfl);

  JDEP_KIND (new) = kind;
  JDEP_DECL (new) = decl;
  JDEP_SOLV (new) = ptr;
  JDEP_WFL (new) = wfl;
  JDEP_CHAIN (new) = NULL;
  JDEP_MISC (new) = NULL_TREE;
  JDEP_GET_PATCH (new) = (tree *)NULL;

  JDEP_INSERT (ctxp->classd_list, new);

  return ptr;
}

void
java_check_circular_reference ()
{
  tree current;
  for (current = ctxp->class_list; current; current = TREE_CHAIN (current))
    {
      tree type = TREE_TYPE (current);
      if (CLASS_INTERFACE (TYPE_NAME (type)))
	{
	  /* Check all interfaces this class extends */
	  tree basetype_vec = TYPE_BINFO_BASETYPES (type);
	  int n, i;

	  if (!basetype_vec)
	    return;
	  n = TREE_VEC_LENGTH (basetype_vec);
	  for (i = 0; i < n; i++)
	    {
	      tree vec_elt = TREE_VEC_ELT (basetype_vec, i);
	      if (vec_elt && BINFO_TYPE (vec_elt) != object_type_node 
		  && interface_of_p (type, BINFO_TYPE (vec_elt)))
		parse_error_context (lookup_cl (current),
				     "Cyclic interface inheritance");
	    }
	}
      else
	if (inherits_from_p (CLASSTYPE_SUPER (type), type))
	  parse_error_context (lookup_cl (current), 
			       "Cyclic class inheritance");
    }
}

void
safe_layout_class (class)
     tree class;
{
  tree save_current_class = current_class;
  char *save_input_filename = input_filename;
  int save_lineno = lineno;
  
  push_obstacks (&permanent_obstack, &permanent_obstack);
  layout_class (class);
  pop_obstacks ();
  
  current_class = save_current_class;
  input_filename = save_input_filename;
  lineno = save_lineno;
  CLASS_LOADED_P (class) = 1;
}

static tree
jdep_resolve_class (dep)
     jdep *dep;
{
  tree decl;

  if (!JDEP_RESOLVED_P (dep))
    {
      decl = 
	resolve_class (JDEP_TO_RESOLVE (dep), JDEP_DECL (dep), JDEP_WFL (dep));
      JDEP_RESOLVED (dep, decl);
    }
  else
    decl = JDEP_RESOLVED_DECL (dep);

  if (!decl)
    {
      complete_class_report_errors (dep);
      return NULL_TREE;
    }
  return decl;
}

/* Complete unsatisfied class declaration and their dependencies */

void
java_complete_class ()
{
  tree current;
  tree cclass;
  jdeplist *cclassd;
  int error_found;

  push_obstacks (&permanent_obstack, &permanent_obstack);

  /* Process imports and reverse the import on demand list */
  process_imports ();
  if (ctxp->import_demand_list)
    ctxp->import_demand_list = nreverse (ctxp->import_demand_list);

  /* Rever things so we have the right order */
  ctxp->class_list = nreverse (ctxp->class_list);
  ctxp->classd_list = reverse_jdep_list (ctxp);
    
  for (cclassd = ctxp->classd_list, cclass = ctxp->class_list; 
       cclass && cclassd; 
       cclass = TREE_CHAIN (cclass), cclassd = CLASSD_CHAIN (cclassd))
    {
      jdep *dep;
      for (dep = CLASSD_FIRST (cclassd); dep; dep = JDEP_CHAIN (dep))
	{
	  tree decl;

	  if (!(decl = jdep_resolve_class (dep)))
	    continue;

	  /* Now it's time to patch */
	  switch (JDEP_KIND (dep))
	    {
	    case JDEP_SUPER:
	      /* Simply patch super */
	      if (parser_check_super (decl, JDEP_DECL (dep), JDEP_WFL (dep)))
		continue;
	      BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (TYPE_BINFO 
	        (TREE_TYPE (JDEP_DECL (dep)))), 0)) = TREE_TYPE (decl);
	      break;

	    case JDEP_FIELD:
	      {
		/* We do part of the job done in add_field */
		tree field_decl = JDEP_DECL (dep);
		tree field_type = TREE_TYPE (decl);
		push_obstacks (&permanent_obstack, &permanent_obstack);
#if ! JAVA_PROMOTE_TO_INT
		if (TREE_CODE (field_type) == RECORD_TYPE)
#endif
		  field_type = promote_type (field_type);
		pop_obstacks ();
		TREE_TYPE (field_decl) = field_type;
		SOURCE_FRONTEND_DEBUG 
		  (("Completed field/var decl `%s' with `%s'",
		    IDENTIFIER_POINTER (DECL_NAME (field_decl)),
		    IDENTIFIER_POINTER (DECL_NAME (decl))));
		break;
	      }
	    case JDEP_METHOD:	/* We start patching a method */
	    case JDEP_METHOD_RETURN:
	      error_found = 0;
	      while (1)
		{
		  if (decl)
		    {
		      tree type = promote_type (TREE_TYPE(decl));
		      JDEP_APPLY_PATCH (dep, type);
		      SOURCE_FRONTEND_DEBUG 
			(((JDEP_KIND (dep) == JDEP_METHOD_RETURN ?
			   "Completing fct `%s' with ret type `%s'":
			   "Completing arg `%s' with type `%s'"),
			  IDENTIFIER_POINTER (EXPR_WFL_NODE 
					      (JDEP_DECL_WFL (dep))),
			  IDENTIFIER_POINTER (DECL_NAME (decl))));
		    }
		  else
		    error_found = 1;
		  dep = JDEP_CHAIN (dep);
		  if (JDEP_KIND (dep) == JDEP_METHOD_END)
		    break;
		  else
		    decl = jdep_resolve_class (dep);
		}
	      if (!error_found)
		{
		  tree mdecl = JDEP_DECL (dep), signature;
		  push_obstacks (&permanent_obstack, &permanent_obstack);
		  /* Recompute and reset the signature */
		  signature = build_java_signature (TREE_TYPE (mdecl));
		  set_java_signature (TREE_TYPE (mdecl), signature);
		  pop_obstacks ();
		}
	      else
		continue;
	      break;

	    case JDEP_INTERFACE:
	      if (parser_check_super_interface (decl, JDEP_DECL (dep),
						JDEP_WFL (dep)))
		continue;
	      parser_add_interface (JDEP_DECL (dep), decl, JDEP_WFL (dep));
	      break;

	    case JDEP_VARIABLE:
	      JDEP_APPLY_PATCH (dep, promote_type (TREE_TYPE (decl)));
	      SOURCE_FRONTEND_DEBUG 
		(("Completing variable `%s' with type `%s'",
		  (TREE_CODE (JDEP_DECL_WFL (dep)) == EXPR_WITH_FILE_LOCATION ?
		   IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_DECL_WFL (dep))) :
		   IDENTIFIER_POINTER (DECL_NAME (JDEP_DECL_WFL (dep)))),
		  IDENTIFIER_POINTER (DECL_NAME (decl))));
	      break;

	    case JDEP_TYPE:
	      JDEP_APPLY_PATCH (dep, TREE_TYPE (decl));
	      SOURCE_FRONTEND_DEBUG 
		(("Completing a random type dependency on a '%s' node",
		  tree_code_name [TREE_CODE (JDEP_DECL (dep))]));
	      break;

	    case JDEP_PARM:
	      JDEP_APPLY_PATCH (dep, promote_type (TREE_TYPE (decl)));
	      SOURCE_FRONTEND_DEBUG 
		(("Completing parameter `%s' with type `%s'",
		  IDENTIFIER_POINTER (JDEP_MISC (dep)),
		  IDENTIFIER_POINTER (DECL_NAME (decl))));
	      break;

	    default:
	      fatal ("incomplete switch - java_complete_class");
	    }
	}
    }
  pop_obstacks ();
  return;
}

/* Resolve class CLASS_TYPE. Handle the case of trying to resolve an
   array.  */

static tree
resolve_class (class_type, decl, cl)
     tree class_type, decl, cl;
{
  char *name = IDENTIFIER_POINTER (TYPE_NAME (class_type));
  char *base = name;
  tree resolved_type, resolved_type_decl;
  
  /* 1- Check to see if we have an array. If true, find what we really
     want to resolve  */
  while (name[0] == '[')
    name++;
  if (base != name)
    TYPE_NAME (class_type) = get_identifier (name);

  /* 2- Resolve the bare type */
  if (!(resolved_type_decl = do_resolve_class (class_type, decl, cl)))
    return NULL_TREE;
  resolved_type = TREE_TYPE (resolved_type_decl);

  /* 3- If we have and array, reconstruct the array down to its nesting */
  if (base != name)
    {
      while (base != name)
	{
	  if (TREE_CODE (resolved_type) == RECORD_TYPE)
	    resolved_type  = promote_type (resolved_type);
	  resolved_type = build_java_array_type (resolved_type, -1);
	  name--;
	}
      /* Build a fake decl for this, since this is what is expected to
         be returned.  */
      resolved_type_decl =
	build_decl (TYPE_DECL, TYPE_NAME (resolved_type), resolved_type);
      /* Figure how those two things are important for error report. FIXME */
      DECL_SOURCE_LINE (resolved_type_decl) = 0;
      DECL_SOURCE_FILE (resolved_type_decl) = input_filename;
    }
  return resolved_type_decl;
}

/* Effectively perform the resolution of class CLASS_TYPE. DECL or CL
   are used to report error messages.  */

static tree
do_resolve_class (class_type, decl, cl)
     tree class_type;
     tree decl;
     tree cl;
{
  tree new_class_decl;
  tree original_name = NULL_TREE;

  /* Do not try to replace TYPE_NAME (class_type) by a variable, since
     its is changed by find_in_imports{_on_demand} */

  /* 1- Check for the type in single imports */
  if (find_in_imports (class_type))
    return NULL_TREE;

  /* 2- And check for the type in the current compilation unit. If it fails,
     try with a name qualified with the package name if appropriate. */

  if ((new_class_decl = IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type))))
    {
      if (!CLASS_LOADED_P (TREE_TYPE (new_class_decl)) &&
	  !CLASS_FROM_SOURCE_P (TREE_TYPE (new_class_decl)))
	load_class (TYPE_NAME (class_type), 0);
      return IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type));
    }

  original_name = TYPE_NAME (class_type);
  if (!QUALIFIED_P (TYPE_NAME (class_type)) && ctxp->package)
    TYPE_NAME (class_type) = merge_qualified_name (ctxp->package, 
						   TYPE_NAME (class_type));
  if ((new_class_decl = IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type))))
    {
      if (!CLASS_LOADED_P (TREE_TYPE (new_class_decl)) &&
	  !CLASS_FROM_SOURCE_P (TREE_TYPE (new_class_decl)))
	load_class (TYPE_NAME (class_type), 0);
      return IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type));
    }
  TYPE_NAME (class_type) = original_name;

  /* 3- Check an other compilation unit that bears the name of type */
  load_class (TYPE_NAME (class_type), 0);
  if (check_pkg_class_access (TYPE_NAME (class_type), 
			      (cl ? cl : lookup_cl (decl))))
    return NULL_TREE;

  if ((new_class_decl = IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type))))
    return new_class_decl;

  /* 4- Check the import on demands. Don't allow bar.baz to be
     imported from foo.* */
  if (!QUALIFIED_P (TYPE_NAME (class_type)))
    if (find_in_imports_on_demand (class_type))
      return NULL_TREE;

  /* 5- Last call for a resolution */
  return IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type));
}

/* Resolve NAME and lay it out (if not done and if not the current
   parsed class). Return a decl node.  */

static tree
resolve_and_layout (name, cl)
     tree name;
     tree cl;
{
  tree decl = resolve_no_layout (name, cl);
  if (decl && TREE_TYPE (decl) != current_class 
      && !CLASS_LOADED_P (TREE_TYPE (decl)))
    safe_layout_class (TREE_TYPE (decl));
  return decl;
}

/* Resolve a class, returns its decl but doesn't perform any
   layout. The current parsing context is saved and restored */

static tree
resolve_no_layout (name, cl)
     tree name, cl;
{
  tree ptr, decl;
  BUILD_PTR_FROM_NAME (ptr, name);
  java_parser_context_save_global ();
  decl = resolve_class (ptr, NULL_TREE, cl);
  java_parser_context_restore_global ();
  
  return decl;
}

/* Called to report errors. Skip leader '[' in a complex array type
   description that failed to be resolved. */

static char *
purify_type_name (name)
     char *name;
{
  while (*name && *name == '[')
    name++;
  return name;
}

/* The type CURRENT refers to can't be found. We print error messages.  */

static void
complete_class_report_errors (dep)
     jdep *dep;
{
  switch (JDEP_KIND (dep))
    {
    case JDEP_SUPER:
      parse_error_context  
	(JDEP_WFL (dep), "Superclass `%s' of class `%s' not found",
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep))),
	 IDENTIFIER_POINTER (DECL_NAME (JDEP_DECL (dep))));
      break;
    case JDEP_FIELD:
      parse_error_context
	(JDEP_WFL (dep), "Type `%s' not found in declaration of field `%s'",
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep))),
	 IDENTIFIER_POINTER (DECL_NAME (JDEP_DECL (dep))));
      break;
    case JDEP_METHOD:		/* Covers arguments */
      parse_error_context
	(JDEP_WFL (dep), "Type `%s' not found in the declaration of the "
	 "argument `%s' of method `%s'",
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep))),
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_DECL_WFL (dep))),
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_MISC (dep))));
      break;
    case JDEP_METHOD_RETURN:	/* Covers return type */
      parse_error_context
	(JDEP_WFL (dep), "Type `%s' not found in the declaration of the "
	 "return type of method `%s'", 
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep))),
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_DECL_WFL (dep))));
      break;
    case JDEP_INTERFACE:
      parse_error_context
	(JDEP_WFL (dep), "Superinterface `%s' of %s `%s' not found",
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep))),
	 (CLASS_OR_INTERFACE (JDEP_DECL (dep), "class", "interface")),
	 IDENTIFIER_POINTER (DECL_NAME (JDEP_DECL (dep))));
      break;
    case JDEP_VARIABLE:
      parse_error_context
	(JDEP_WFL (dep), "Type `%s' not found in the declaration of the "
	 "local variable `%s'", 
	 purify_type_name (IDENTIFIER_POINTER (EXPR_WFL_NODE (JDEP_WFL (dep)))),
	 IDENTIFIER_POINTER (DECL_NAME (JDEP_DECL (dep))));
      break;
    }
}

/* Check uninitialized final.  */

void
java_check_final ()
{
}

static int
check_method_redefinition (class, method)
     tree class, method;
{
  tree redef, name;
  tree cl = DECL_NAME (method);
  tree sig = TYPE_LANG_SPECIFIC (TREE_TYPE (method))->signature;
  /* decl name of generated <clinit> doesn't need to be fixed and
     checked */
  if (DECL_NAME (method) != clinit_identifier_node)
    {
      /* NAME is just the plain name when Object is being defined */
      if (class != object_type_node)
	name = DECL_NAME (method) = EXPR_WFL_NODE (DECL_NAME (method));
      else
	name = DECL_NAME (method);
    }
  else 
    return 0;
  
  for (redef = TYPE_METHODS (class); redef; redef = TREE_CHAIN (redef))
    {
      struct lang_type *t = TYPE_LANG_SPECIFIC (TREE_TYPE (redef));
      
      if (! t || (redef == method))
	break;
      if (DECL_NAME (redef) == name && sig == t->signature)
	{
	  parse_error_context (cl, "Duplicate method declaration");
	  return 1;
	}
    }
  return 0;
}

/* Check all the methods of CLASS. Methods are first completed then
   checked according to regular method existance rules.
   If no constructor were encountered, then build its declaration. */

static void
java_check_regular_methods (class_decl)
     tree class_decl;
{
  tree method;
  tree class = CLASS_TO_HANDLE_TYPE (TREE_TYPE (class_decl));
  tree super_class = CLASSTYPE_SUPER (class);
  int seen_constructor = 0;

  TYPE_METHODS (class) = nreverse (TYPE_METHODS (class));

  /* Should take interfaces into account. FIXME */
  for (method = TYPE_METHODS (class); method; method = TREE_CHAIN (method))
    {
      tree found, sig;
      tree method_wfl = DECL_NAME (method);
      int aflags;

      if (DECL_CONSTRUCTOR_P (method))
	seen_constructor = 1;

      /* Check for redefinitions */
      if (check_method_redefinition (class, method))
	continue;

      sig = build_java_argument_signature (TREE_TYPE (method));

      found = lookup_argument_method (super_class, DECL_NAME (method), sig);
      if (! found)
        continue;
      /* Can't override a method with the same name and different return
	 types. */
      if (TREE_TYPE (TREE_TYPE (found)) != TREE_TYPE (TREE_TYPE (method)))
	parse_warning_context 
	  (method_wfl,
	   "Method `%s' redefined with different return type in class `%s'",
	   lang_printable_name (found),
	   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));

      /* Can't override final. Can't override static. */
      if (METHOD_FINAL (found) || METHOD_STATIC (found))
	{
	  /* Static *can* override static */
	  if (METHOD_STATIC (found) && METHOD_STATIC (method))
	    continue;
	  parse_error_context 
	    (method_wfl,
	     "%s methods can't be overriden. Method `%s' is %s in class `%s'",
	     (METHOD_FINAL (found) ? "Final" : "Static"),
	     lang_printable_name (found),
	     (METHOD_FINAL (found) ? "final" : "static"),
	     IDENTIFIER_POINTER
	       (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));
	  continue;
	}
      /* Static method can't override instance method. */
      if (METHOD_STATIC (method))
	{
	  parse_error_context 
	    (method_wfl,
	     "Instance methods can't be overriden by a static method. Method "
	     "`%s' is an instance method in class `%s'",
	     lang_printable_name (found),
	     IDENTIFIER_POINTER
	       (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));
	  continue;
	}
      /* Overriding/hiding public must be public or
	 overriding/hiding protected must be protected or public */
      if ((METHOD_PUBLIC (found) && !METHOD_PUBLIC (method)) ||
	  (METHOD_PROTECTED (found) 
	   && !(METHOD_PUBLIC (method) || METHOD_PROTECTED (method))))
	{
	  parse_error_context 
	    (method_wfl,
	     "Methods can't be overridden to be more private. Method `%s' is "
	     "%s in class `%s'", lang_printable_name (found),
	     (METHOD_PUBLIC (found) ? "public" : "protected"),
	     IDENTIFIER_POINTER 
	       (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));
	  continue;
	}

      /* If the method has default access in an other package, then
      issue a warning that the current method doesn't override the one
      that was found elsewhere */
      aflags = get_access_flags_from_decl (found);
      if ((!aflags || (aflags > ACC_PROTECTED))
	  && !class_in_current_package (DECL_CONTEXT (found)))
	parse_warning_context 
	  (method_wfl, "Method `%s' in class `%s' does not "
	   "override the corresponding method in class `%s', which is "
	   "private to a different package",
	   lang_printable_name (found),
	   IDENTIFIER_POINTER (DECL_NAME (class_decl)),
	   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));

      /* Check on (default) package access. FIXME.  */
      /* Inheriting multiple methods with the same signature. FIXME */
    }
  
  TYPE_METHODS (class) = nreverse (TYPE_METHODS (class));

  if (!seen_constructor)
    {
      /* No constructor seen, we craft one, at line 0 */
      int saved_lineno = lineno;
      tree meth, decl;
      lineno = 0;
      meth = make_node (FUNCTION_TYPE);
      TREE_TYPE (meth) = void_type_node;
      TYPE_ARG_TYPES (meth) = NULL_TREE; 
      decl = add_method (class, 0, init_identifier_node,
			 build_java_signature (meth));
      DECL_CONSTRUCTOR_P (decl) = 1;
      lineno = saved_lineno;
    }
}

/* Check abstract method of interface INTERFACE */

static void
java_check_abstract_methods (interface)
     tree interface;
{
  int i, n;
  tree method, basetype_vec, found;

  for (method = TYPE_METHODS (interface); method; method = TREE_CHAIN (method))
    {
      char *csig;
      tree name = DECL_NAME (method);

      /* 2- Check for double definition inside the defining interface */
      if (check_method_redefinition (interface, method))
	continue;

      /* 3- Overriding is OK as far as we preserve the return type and
	 the thrown exceptions */
      found = lookup_java_interface_method2 (interface, method);
      if (found)
	{
	  parse_error_context 
	    (lookup_cl (method),
	     "Method `%s' previously defined in interface `%s' is "
	     "redefined with different return type in interface `%s'",
	     lang_printable_name (found),
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))),
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (interface))));
	  continue;
	}
    }

  /* 4- Inherited methods can't differ by their returned types */
  if (!(basetype_vec = TYPE_BINFO_BASETYPES (interface)))
    return;
  n = TREE_VEC_LENGTH (basetype_vec);
  for (i = 0; i < n; i++)
    {
      tree sub_interface_method, sub_interface;
      tree vec_elt = TREE_VEC_ELT (basetype_vec, i);
      if (!vec_elt)
	continue;
      sub_interface = BINFO_TYPE (vec_elt);
      for (sub_interface_method = TYPE_METHODS (sub_interface); 
	   sub_interface_method;
	   sub_interface_method = TREE_CHAIN (sub_interface_method))
	{
	  found = lookup_java_interface_method2 (interface, 
						 sub_interface_method);
	  if (found && (found != sub_interface_method))
	    parse_error_context 
	      (lookup_cl (sub_interface_method),
	       "Interface `%s' inherits method `%s' from interface `%s'. This "
	       "method is redefined with a different return "
	       "type in interface `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (interface))),
	       lang_printable_name (found),
	       IDENTIFIER_POINTER 
	         (DECL_NAME (TYPE_NAME (DECL_CONTEXT (sub_interface_method)))),
	       IDENTIFIER_POINTER 
	         (DECL_NAME (TYPE_NAME (DECL_CONTEXT (found)))));
	}
    }
}

/* Check the method on all the defined classes. Should be done to the
   classes declared in the compilation unit only. FIXME  */

void
java_check_methods ()
{

  tree current;
  for (current = ctxp->class_list; current; current = TREE_CHAIN (current))
    if (CLASS_FROM_SOURCE_P (TREE_TYPE (current)))
      {
	tree class = CLASS_TO_HANDLE_TYPE (TREE_TYPE (current));

	if (CLASS_INTERFACE (TYPE_NAME (class)))
	  java_check_abstract_methods (class);
	else
	  java_check_regular_methods (current);
      }
}

/* Lookup methods in interfaces using their name and partial
   signature. Return a matching method only if their types differ.  */

static tree
lookup_java_interface_method2 (class, method_decl)
     tree class, method_decl;
{
  int i, n;
  tree basetype_vec = TYPE_BINFO_BASETYPES (class), to_return;

  if (!basetype_vec)
    return NULL_TREE;

  n = TREE_VEC_LENGTH (basetype_vec);
  for (i = 0; i < n; i++)
    {
      tree vec_elt = TREE_VEC_ELT (basetype_vec, i), to_return;
      if ((BINFO_TYPE (vec_elt) != object_type_node)
	  && (to_return = 
	      lookup_java_method2 (BINFO_TYPE (vec_elt), method_decl, 1)))
	return to_return;
    }
  for (i = 0; i < n; i++)
    {
      to_return = lookup_java_interface_method2 
	(BINFO_TYPE (TREE_VEC_ELT (basetype_vec, i)), method_decl);
      if (to_return)
	return to_return;
    }

  return NULL_TREE;
}

/* Lookup method using their name and partial signature. Return a
   matching method only if their types differ.  */

static tree
lookup_java_method2 (clas, method_decl, do_interface)
     tree clas, method_decl;
     int do_interface;
{
  tree method, method_signature, method_name, method_type;
  method_signature = build_java_argument_signature (TREE_TYPE (method_decl));
  method_name = DECL_NAME (method_decl);
  method_type = TREE_TYPE (TREE_TYPE (method_decl));

  while (clas != NULL_TREE)
    {
      for (method = TYPE_METHODS (clas);
	   method != NULL_TREE;  method = TREE_CHAIN (method))
	{
	  tree method_sig = build_java_argument_signature (TREE_TYPE (method));
	  if (DECL_NAME (method) == method_name 
	      && method_sig == method_signature 
	      && TREE_TYPE (TREE_TYPE (method)) != method_type)
	    {
	      return method;
	    }
	}
      clas = (do_interface ? NULL_TREE : CLASSTYPE_SUPER (clas));
    }
  return NULL_TREE;
}

/* Return the line that matches DECL line number. Used during error
   report */

static tree
lookup_cl (decl)
     tree decl;
{
  static tree cl = NULL_TREE;
  
  if (!decl)
    return NULL_TREE;

  if (cl == NULL_TREE)
    cl = build_expr_wfl (NULL_TREE, NULL, 0, 0);

  EXPR_WFL_FILENAME_NODE (cl) = get_identifier (DECL_SOURCE_FILE (decl));
  EXPR_WFL_SET_LINECOL (cl, DECL_SOURCE_LINE_FIRST (decl), -1);

  return cl;
}

/* Look for a simple name in the single-type import list */

static tree
find_name_in_single_imports (name)
     tree name;
{
  tree node;

  for (node = ctxp->import_list; node; node = TREE_CHAIN (node))
    if (TREE_VALUE (node) == name)
      return (EXPR_WFL_NODE (TREE_PURPOSE (node)));

  return NULL_TREE;
}

/* Process all single-type import. */

static int
process_imports ()
{
  tree import;
  int error_found;

  for (import = ctxp->import_list; import; import = TREE_CHAIN (import))
    {
      tree to_be_found = EXPR_WFL_NODE (TREE_PURPOSE (import));

      /* Don't load twice something already defined. */
      if (IDENTIFIER_CLASS_VALUE (to_be_found))
	continue;
      QUALIFIED_P (to_be_found) = 1;
      load_class (to_be_found, 0);
      error_found =
	check_pkg_class_access (to_be_found, TREE_PURPOSE (import));
      if (!IDENTIFIER_CLASS_VALUE (to_be_found))
	{
	  parse_error_context (TREE_PURPOSE (import),
			       "Class or interface `%s' not found in import",
			       IDENTIFIER_POINTER (to_be_found));
	  return 1;
	}
      if (error_found)
	return 1;
    }
  return 0;
}

/* Possibly find a class imported by a single-type import statement. Return
   1 if an error occured, 0 otherwise. */

static int
find_in_imports (class_type)
     tree class_type;
{
  tree import;

  for (import = ctxp->import_list; import; import = TREE_CHAIN (import))
    if (TREE_VALUE (import) == TYPE_NAME (class_type))
      {
	TYPE_NAME (class_type) = EXPR_WFL_NODE (TREE_PURPOSE (import));
	QUALIFIED_P (TYPE_NAME (class_type)) = 1;
	return check_pkg_class_access (TYPE_NAME (class_type), 
				       TREE_PURPOSE (import));
      }
  return 0;
}

/* Process a import on demand statement (lazy) */

static int
read_import_entry (jcf, dirp, returned_name)
     JCF *jcf;
     DIR *dirp;
     char **returned_name;
{
  if (dirp)
    {
      struct dirent *direntp = readdir (dirp);
      if (!direntp)
	{
	  *returned_name = NULL;
	  return 0;
	}
      else
	{
	  *returned_name = direntp->d_name;
	  return (strlen (direntp->d_name));
	}
    }
  else
    {
      int current_dir_len = strlen (jcf->classname);
      char *current_entry;
      int current_entry_len;

      /* Here we read a zip directory as a file directory. The files
	 we're selecting must have the same root than the directory
	 we're examining. */

      ZipDirectory *zipd = (ZipDirectory *)jcf->zipd; 

      while (zipd)
	{
	  current_entry = ZIPDIR_FILENAME (zipd);
	  current_entry_len = zipd->filename_length;
	  while (current_entry_len && current_entry [current_entry_len] != '/')
	    current_entry_len--;
	  /* If the path of the current file doesn't match the directory we're
	     scanning, that the end of the search */
	  current_entry_len++;
	  if (strncmp (jcf->classname, current_entry, current_dir_len))
	    {
	      *returned_name = NULL;
	      return 0;
	    }
	  /* Ok, we have at least the same path. The position of the last '/'
	     of the current file we're examining should match the size of
	     name of the directory we're browsing, otherwise that an entry
	     belonging to a sub directory, we want to skip it. */
	  if (current_entry_len != current_dir_len)
	    zipd = ZIPDIR_NEXT (zipd);
	  else
	    {
	      jcf->zipd = ZIPDIR_NEXT (zipd); /* Prepare next read */
	      *returned_name = &current_entry [current_entry_len];
	      return (zipd->filename_length - current_entry_len);
	    }
	}
    }
}

/* Read a import directory, gathering potential match for further type
   references. Indifferently reads a filesystem or a ZIP archive
   directory.  */

static void
read_import_dir (wfl)
     tree wfl;
{
  char *name = IDENTIFIER_POINTER (EXPR_WFL_NODE (wfl));
  int name_len = IDENTIFIER_LENGTH (EXPR_WFL_NODE (wfl)), reclen;
  DIR *dirp = NULL;
  tree dirname = ident_subst (name, name_len, "", '.', '/', "");
  JCF jcfr, *jcf, *saved_jcf = current_jcf;
  char *founddirname, *d_name;
  struct ZipFileCache zip_cache;

  jcf = &jcfr;
  if (!classpath)
    fix_classpath ();
  if (!(founddirname = find_class (name, name_len, jcf, 0)))
    fatal ("Can't import `%s'", name);
  if (jcf->outofsynch)
    jcf_out_of_synch (jcf);
  if (jcf->seen_in_zip)
    jcf->zipd = ZIPDIR_NEXT ((ZipDirectory *)jcf->zipd);

  else if (founddirname && (dirp = opendir (founddirname)))
    {
      readdir (dirp); readdir (dirp);
    }

  if (!founddirname && !dirp)
    {
      static int first = 1;
      if (first)
	{
	  char buffer [256];
	  sprintf (buffer, "Can't find default package `%s'. Check "
		   "the CLASSPATH environment variable and the access to the "
		   "archives.", name);
	  error (buffer);
	  java_error_count++;
	  first = 0;
	}
      else
	parse_error_context (wfl, "Package `%s' not found in import", name);
      current_jcf = saved_jcf;
      return;
    }

  /* Here we should have a unified way of retrieving an entry, to be
     indexed. */
  while ((reclen = read_import_entry (jcf, dirp, &d_name)))
    {
      int java_or_class = 0;
      int len; 
      if ((reclen > 5) 
	  && !strcmp (&d_name [reclen-5], ".java"))
	{
	  java_or_class = 1;
	  len = reclen - 5;
	}
	  
      if (!java_or_class && (reclen > 6) &&
	  !strcmp (&d_name [reclen-6], ".class"))
	{
	  java_or_class = 2;
	  len = reclen - 6;
	}

      if (java_or_class)
	{
	  char *id_name;
	  tree node, old;

	  obstack_grow (&temporary_obstack, name, name_len);
	  obstack_1grow (&temporary_obstack, '/');
	  obstack_grow0 (&temporary_obstack, d_name, len);
	  id_name = obstack_finish (&temporary_obstack);

	  node = get_identifier (id_name);
	  IS_A_CLASSFILE_NAME (node) = 1; /* Or soon to be */
	  QUALIFIED_P (node) = 1; /* As soon as we turn / into . */
	}
    }
  if (dirp)
    closedir (dirp);
  
  current_jcf = saved_jcf;
}

/* Possibly find a type in the import on demands specified
   types. Returns 1 if an error occured, 0 otherwise. Run throught the
   entire list, to detected potential double definitions.  */
		 
static int
find_in_imports_on_demand (class_type)
     tree class_type;
{
  tree node, import, node_to_use;
  int seen_once = -1;
  tree cl;

  for (import = ctxp->import_demand_list; import; import = TREE_CHAIN (import))
    {
      char *id_name;
      tree found;
      obstack_grow (&temporary_obstack, 
		    IDENTIFIER_POINTER (EXPR_WFL_NODE (TREE_PURPOSE (import))),
		    IDENTIFIER_LENGTH (EXPR_WFL_NODE (TREE_PURPOSE (import))));
      obstack_1grow (&temporary_obstack, '/');
      obstack_grow0 (&temporary_obstack, 
		     IDENTIFIER_POINTER (TYPE_NAME (class_type)),
		     IDENTIFIER_LENGTH (TYPE_NAME (class_type)));
      id_name = obstack_finish (&temporary_obstack);
	      
      node = maybe_get_identifier (id_name);
      if (node && IS_A_CLASSFILE_NAME (node))
	{
	  if (seen_once < 0)
	    {
	      cl = TREE_PURPOSE (import);
	      seen_once = 1;
	      node_to_use = node;
	    }
	  else
	    {
	      seen_once++;
	      parse_error_context 
		(import, "Type `%s' also potentially defined in package `%s'",
		 IDENTIFIER_POINTER (TYPE_NAME (class_type)),
		 IDENTIFIER_POINTER (EXPR_WFL_NODE (TREE_PURPOSE (import))));
	    }
	}
    }

  if (seen_once == 1)
    {
      /* Setup lineno so that it refers to the line of the import (in
	 case we parse a class file and encounter errors */
      tree decl;
      int saved_lineno = lineno;
      lineno = EXPR_WFL_LINENO (cl);
      TYPE_NAME (class_type) = ident_subst (IDENTIFIER_POINTER (node_to_use),
					    IDENTIFIER_LENGTH (node_to_use),
					    "", '/', '.', "");
      QUALIFIED_P (TYPE_NAME (class_type)) = 1;
      decl = IDENTIFIER_CLASS_VALUE (TYPE_NAME (class_type));
      /* If there is no DECL set for the class or if the class isn't
	 loaded and not seen in source yet, the load */
      if (!decl || (!CLASS_LOADED_P (TREE_TYPE (decl))
		    && !CLASS_FROM_SOURCE_P (TREE_TYPE (decl))))
	load_class (node_to_use, 0);
      lineno = saved_lineno;
      return check_pkg_class_access (TYPE_NAME (class_type), cl);
    }
  else
    return (seen_once < 0 ? 0 : seen_once); /* It's ok not to have found */
}

/* Check that CLASS_NAME refers to a PUBLIC class. Return 0 if no
   access violations were found, 1 otherwise.  */

static int
check_pkg_class_access (class_name, cl)
     tree class_name;
     tree cl;
{
  tree type;
  int access;

  if (!QUALIFIED_P (class_name) || !IDENTIFIER_CLASS_VALUE (class_name))
    return 0;

  if (!(type = TREE_TYPE (IDENTIFIER_CLASS_VALUE (class_name))))
    return 0;

  if (!CLASS_PUBLIC (TYPE_NAME (type)))
    {
      parse_error_context 
	(cl, "Can't access %s `%s'. Only public classes and interfaces in "
	 "other packages can be accessed",
	 (CLASS_INTERFACE (TYPE_NAME (type)) ? "interface" : "class"),
	 IDENTIFIER_POINTER (class_name));
      return 1;
    }
  return 0;
}

/* Local variable declaration. */

static void
declare_local_variables (modifier, type, vlist)
     int modifier;
     tree type;
     tree vlist;
{
  tree decl, current, returned_type, type_wfl, init_stmt = NULL_TREE;
  int must_chain = 0;

  /* Push a new block if statement were seen between the last time we
     pushed a block and now. Keep a cound of block to close */
  if (BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (current_function_decl)))
    {
      tree body = DECL_FUNCTION_BODY (current_function_decl);
      tree b = enter_block ();
      BLOCK_EXPR_ORIGIN(b) = body;
    }

  if (modifier)
    {
      int i;
      for (i = 0; i <= 10; i++) if (1 << i & modifier) break;
      parse_error_context 
	(ctxp->modifier_ctx [i],
	 (modifier == ACC_FINAL ?
	  "Unsupported JDK1.1 `final' locals" :
	  "Only `final' is allowed as a local variables modifier"));
      return;
    }

  if (unresolved_type_p (type, &returned_type))
    {
      if (returned_type)
        type = returned_type;
      else 
	{
	  type_wfl = type;
	  type = obtain_incomplete_type (type);
	  must_chain = 1;
	}
    }
  
  for (current = vlist; current; current = TREE_CHAIN (current))
    {
      tree wfl  = TREE_PURPOSE (current);
      tree name = EXPR_WFL_NODE (wfl);
      tree init = TREE_VALUE (current);
      tree other = lookup_name_in_blocks (name);

      /* Don't try to use an INIT statement when an error was found */
      if (init && java_error_count)
	init = NULL_TREE;

      if (other)
	parse_error_context 
	  (wfl, "Variable `%s' is already defined in this method and was "
	   "declared `%s %s' in line %d", 
	   IDENTIFIER_POINTER (name), lang_printable_name (TREE_TYPE (other)),
	   IDENTIFIER_POINTER (name), DECL_SOURCE_LINE (other));
      else
	{
	  if (!must_chain && TREE_CODE (type) == RECORD_TYPE)
	    type = promote_type (type);
	  /* Never layout this decl. This will be done when its scope
             will be entered */
	  decl = build_decl_no_layout (VAR_DECL, name, type);
	  BLOCK_CHAIN_DECL (decl);

	  /* Add the initialization function to the current function's code */
	  if (init)
	    {
	      tree wfl;
	      MODIFY_EXPR_FROM_INITIALIZATION_P (init) = 1;
	      java_method_add_stmt 
		(current_function_decl,
		 build_debugable_stmt (EXPR_WFL_LINECOL (init), init));
	    }

	  if (must_chain)
	    {
	      jdep *dep;
	      register_incomplete_type (JDEP_VARIABLE, type_wfl, decl, type);
	      dep = CLASSD_LAST (ctxp->classd_list);
	      JDEP_GET_PATCH (dep) = &TREE_TYPE (decl);
	    }
	}
    }
  SOURCE_FRONTEND_DEBUG (("Defined locals"));
}

/* Called during parsing. Build decls from argument list.  */

static void
source_start_java_method (fndecl)
     tree fndecl;
{
  tree tem;
  tree parm_decl;
  int i;

  extern tree current_binding_level;
  current_function_decl = fndecl;

  /* New scope for the function */
  enter_block ();
  for (tem = TYPE_ARG_TYPES (TREE_TYPE (fndecl)), i = 0;
       tem != NULL_TREE; tem = TREE_CHAIN (tem), i++)
    {
      tree type = TREE_VALUE (tem);
      tree name = TREE_PURPOSE (tem);
      
      /* If type is incomplete. Layout can't take place
	 now. Create an incomplete decl and ask for the decl to be
	 patched later */
      if (INCOMPLETE_TYPE_P (type))
	{
	  jdep *jdep;
	  parm_decl = build_decl_no_layout (PARM_DECL, name, type);
	  
	  register_incomplete_type (JDEP_PARM, NULL_TREE, NULL_TREE, type);
	  jdep = CLASSD_LAST (ctxp->classd_list);
	  JDEP_MISC (jdep) = name;
	  JDEP_GET_PATCH (jdep) = &TREE_TYPE (parm_decl);
	}
      else
	parm_decl = build_decl (PARM_DECL, name, type);

      BLOCK_CHAIN_DECL (parm_decl);
    }
  tem = BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (current_function_decl));
  BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (current_function_decl)) =
    nreverse (tem);
  DECL_ARG_SLOT_COUNT (current_function_decl) = i;
}

/* Called during expansion. Push decls formerly built from argument
   list so they're usable during expansion. */

static void
expand_start_java_method (fndecl)
     tree fndecl;
{
  tree tem, *ptr;
  tree parm_decl;

  extern tree current_binding_level;
  current_function_decl = fndecl;

  announce_function (fndecl);
  pushlevel (1);		/* Push parameters */
  ptr = &DECL_ARGUMENTS (fndecl);
  tem  = BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (current_function_decl));
  while (tem)
    {
      tree next = TREE_CHAIN (tem);
      DECL_ARG_TYPE (tem) = TREE_TYPE (tem);
      layout_decl (tem, 0);
      pushdecl (tem);
      INITIALIZED_P (tem) = 1;	/* Parms are initialized */
      *ptr = tem;
      ptr = &TREE_CHAIN (tem);
      tem = next;
    }
  *ptr = NULL_TREE;
  pushdecl_force_head (DECL_ARGUMENTS (fndecl));
  lineno = DECL_SOURCE_LINE_FIRST (fndecl);
  complete_start_java_method (fndecl); 
}

/* Terminate a function and expand its body.  */

static void
source_end_java_method ()
{
  tree fndecl = current_function_decl;

  java_parser_context_save_global ();
  lineno = ctxp->last_ccb_indent1;

  /* Generate function's code */
  if (BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (fndecl))
      && ! flag_emit_class_files)
    expand_expr_stmt (BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (fndecl)));

  /* pop out of its parameters */
  pushdecl_force_head (DECL_ARGUMENTS (fndecl));
  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Generate rtl for function exit.  */
  if (! flag_emit_class_files)
    {
      lineno = DECL_SOURCE_LINE_LAST (fndecl);
      expand_function_end (input_filename, lineno, 0);

      /* Run the optimizers and output assembler code for this function. */
      rest_of_compilation (fndecl);
    }

  current_function_decl = NULL_TREE;
  /*  permanent_allocation (1); */
  java_parser_context_restore_global ();
}

/* Record EXPR in the current function block. Complements compound
   expression second operand if necessary.  */

tree
java_method_add_stmt (fndecl, expr)
     tree fndecl, expr;
{
  tree body = BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (fndecl));
  tree node;

  if (java_error_count)
    return body;
  if ((node = add_stmt_to_compound (body, NULL_TREE, expr)) == body)
    return body;

  BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (fndecl)) = node;
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* Add STMT to EXISTING if possible, otherwise create a new
   COMPOUND_EXPR and add STMT to it. */

static tree
add_stmt_to_compound (existing, type, stmt)
     tree existing, type, stmt;
{
  tree node;

  if (existing && (TREE_CODE (existing) == COMPOUND_EXPR)
      && TREE_OPERAND (existing, 1) == size_zero_node)
    {
      TREE_OPERAND (existing, 1) = stmt;
      TREE_TYPE (existing) = type;
      return existing;
    }
  else if (existing)
    node = build (COMPOUND_EXPR, type, existing, stmt);
  else
    node = build (COMPOUND_EXPR, type, stmt, size_zero_node);
  
  return node;
}

/* Hold THIS for the scope of the current public method decl.  */
static tree current_this;

/* Layout all class found during parsing */

void
java_layout_classes ()
{
  tree current;
  for (current = ctxp->class_list; current; current = TREE_CHAIN (current))
    {
      current_class = TREE_TYPE (current);
      TYPE_FIELDS (current_class) = nreverse (TYPE_FIELDS (current_class));
      if (!TYPE_SIZE (current_class))
	safe_layout_class (current_class);
    }
}

/* Expand all methods in all registered classes.  */

void
java_complete_expand_methods ()
{
  tree current;
  
  for (current = ctxp->class_list; current; current = TREE_CHAIN (current))
    {
      extern tree current_constant_pool_data_ref;
      tree class_type = CLASS_TO_HANDLE_TYPE (TREE_TYPE (current));
      tree decl;
      int saved_lineno;

      current_class = TREE_TYPE (current);

      /* Initialize a new constant pool */
      init_outgoing_cpool ();

      /* Don't process function bodies in interfaces */
      if (!CLASS_INTERFACE (TYPE_NAME (current_class)))
	for (decl = TYPE_METHODS (class_type); decl; decl = TREE_CHAIN (decl))
	  {
	    current_function_decl = decl;
	    /* Don't generate debug info on line zero when expanding a
	       generated constructor. */
	    if (DECL_CONSTRUCTOR_P (decl) && !DECL_FUNCTION_BODY (decl))
	      {
		/* If we found errors, it's too dangerous to try to generate
		   and expand a constructor */
		if (!java_error_count)
		  {
		    restore_line_number_status (1);
		    java_complete_expand_method (decl);
		    restore_line_number_status (0);
		  }
	      }
	    else
	      java_complete_expand_method (decl);
	  }

      /* Make the class data, register it and run the rest of decl
         compilation on it */
      if (!java_error_count && ! flag_emit_class_files)
	{
	  make_class_data (current_class);
	  register_class ();
	  rest_of_decl_compilation (TYPE_NAME (current_class), (char*) 0, 1, 0);
	}
    }
}

/* Complete and expand a method.  */

static void
java_complete_expand_method (mdecl)
     tree mdecl;
{
  tree node;
  jdep *current;
  int no_ac_found = 1;

  /* We generate some code for an empty constructor */
  if (DECL_CONSTRUCTOR_P (mdecl) && !DECL_FUNCTION_BODY (mdecl))
    {
      tree arg_list, func, call;
      tree method_type = TREE_TYPE (mdecl);
      tree class_type = CLASS_TO_HANDLE_TYPE (current_class);
      tree self_type = (CLASSTYPE_SUPER (class_type) ? 
			CLASSTYPE_SUPER (class_type) : class_type);
      tree method_signature = 
	TYPE_LANG_SPECIFIC (method_type)->signature;
      tree method = 
	lookup_java_constructor (CLASS_TO_HANDLE_TYPE (self_type),
				 method_signature);
      tree block, compound;

      /* Fixe the begining/ending lines of the method so that with
         no_line_numbers set to 1 it doesn't generate debug info at
         line 1 for this artificial constructor. */
      DECL_SOURCE_LINE (mdecl) = 1;
      DECL_SOURCE_LINE_MERGE (mdecl, 1);
      source_start_java_method (mdecl);
      arg_list = BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (mdecl));
      enter_block ();
      func = build_known_method_ref (method, method_type, self_type,
				     method_signature, arg_list);
      
      if (! flag_emit_class_files)
	func = build1 (NOP_EXPR, build_pointer_type (method_type), func);
      call = build (CALL_EXPR, TREE_TYPE (method_type), func, 
		    build_tree_list (NULL_TREE, arg_list), NULL_TREE);
      TREE_SIDE_EFFECTS (call) = 1;
      call = build_class_init (self_type, call);
      compound = java_method_add_stmt (mdecl, call);
      block = exit_block ();
      BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (mdecl)) = block;
      /* The function decl, its block and the compound statement
         within this block are all of void type. */
      TREE_TYPE (block) = TREE_TYPE (compound) = 
	TREE_TYPE (DECL_FUNCTION_BODY (mdecl)) = void_type_node;
      exit_block ();
      no_ac_found = 0;
    }
  
  if (DECL_FUNCTION_BODY (mdecl))
    {
      expand_start_java_method (mdecl);

      current_this 
	= (!METHOD_STATIC (mdecl) ? 
	   BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (mdecl)) : NULL_TREE);

      if (BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (mdecl)) && no_ac_found)
	java_complete_tree (BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (mdecl)));
      /* Don't go any further if we've found error(s) during the
         expansion */
      if (!java_error_count)
	source_end_java_method ();
    }
}

/* Expand finals.  */

void
java_expand_finals ()
{
}

/* Wrap non WFL PRIMARY around a WFL and set EXPR_WFL_QUALIFICATION to
   a tree list node containing RIGHT. Fore coming RIGHTs will be
   chained to this hook. LOCATION contains the location of the
   separating `.' operator.  */

static tree
make_qualified_primary (primary, right, location)
     tree primary, right;
     int location;
{
  tree wfl;

  /* We want to process THIS . xxx symbolicaly, to keep it consistent
     with the way we're processing SUPER. A THIS from a primary as a
     different form than a SUPER. Turn THIS into something symbolic */
  if (TREE_CODE (primary) == JAVA_THIS_EXPR)
    {
      wfl = build_wfl_node (this_identifier_node, input_filename, 0, 0);
      EXPR_WFL_LINECOL (wfl) = EXPR_WFL_LINECOL (primary);
      wfl = make_qualified_name (wfl, right, location);
      PRIMARY_P (wfl) = 1;
      return wfl;
    }
  /* Other non WFL node are wrapped around a WFL */
  else if (TREE_CODE (primary) != EXPR_WITH_FILE_LOCATION)
    {
      wfl = build_expr_wfl (NULL_TREE, ctxp->filename, 0, 0);
      EXPR_WFL_LINECOL (wfl) = EXPR_WFL_LINECOL (primary);
      EXPR_WFL_QUALIFICATION (wfl) = build_tree_list (primary, NULL_TREE);
    }
  else
    {
      wfl = primary;
      if (!EXPR_WFL_QUALIFICATION (primary))
	EXPR_WFL_QUALIFICATION (primary) = 
	  build_tree_list (primary, NULL_TREE);
    }

  EXPR_WFL_LINECOL (right) = location;
  chainon (EXPR_WFL_QUALIFICATION (wfl), build_tree_list (right, NULL_TREE));
  PRIMARY_P (wfl) =  1;
  return wfl;
}

/* Simple merge of two name separated by a `.' */

static tree
merge_qualified_name (left, right)
     tree left, right;
{
  tree node;
  obstack_grow (&temporary_obstack, IDENTIFIER_POINTER (left),
		IDENTIFIER_LENGTH (left));
  obstack_1grow (&temporary_obstack, '.');
  obstack_grow0 (&temporary_obstack, IDENTIFIER_POINTER (right),
		 IDENTIFIER_LENGTH (right));
  node =  get_identifier (obstack_base (&temporary_obstack));
  obstack_free (&temporary_obstack, obstack_base (&temporary_obstack));
  QUALIFIED_P (node) = 1;
  return node;
}

/* Merge the two parts of a qualified name into LEFT.  Set the
   location information of the resulting node to LOCATION, usually
   inherited from the location information of the `.' operator. */

static tree
make_qualified_name (left, right, location)
     tree left, right;
     int location;
{
  int qualified;
  tree left_id = EXPR_WFL_NODE (left);
  tree right_id = EXPR_WFL_NODE (right);
  tree wfl, merge;

  merge = merge_qualified_name (left_id, right_id);

  /* Left wasn't qualified and is now qualified */
  if (!QUALIFIED_P (left_id))
    {
      tree wfl = build_expr_wfl (left_id, ctxp->filename, 0, 0);
      EXPR_WFL_LINECOL (wfl) = EXPR_WFL_LINECOL (left);
      EXPR_WFL_QUALIFICATION (left) = build_tree_list (wfl, NULL_TREE);
    }
  
  wfl = build_expr_wfl (right_id, ctxp->filename, 0, 0);
  EXPR_WFL_LINECOL (wfl) = location;
  chainon (EXPR_WFL_QUALIFICATION (left), build_tree_list (wfl, NULL_TREE));

  EXPR_WFL_NODE (left) = merge;
  return left;
}

/* Extract the last identifier component of the qualified in WFL. The
   last identifier is removed from the linked list */

static tree
cut_identifier_in_qualified (wfl)
     tree wfl;
{
  tree q;
  tree previous = NULL_TREE;
  for (q = EXPR_WFL_QUALIFICATION (wfl); ; previous = q, q = TREE_CHAIN (q))
    if (!TREE_CHAIN (q))
      {
	if (!previous)
	  fatal ("Operating on a non qualified qualified WFL - "
		 "cut_identifier_in_qualified");
	TREE_CHAIN (previous) = NULL_TREE;
	return TREE_PURPOSE (q);
      }
}

/* Resolve the expression name NAME. Return its decl.  */

static tree
resolve_expression_name (id)
     tree id;
{
  tree name = EXPR_WFL_NODE (id);
  tree decl;

  /* 6.5.5.1: Simple expression names */
  if (!PRIMARY_P (id) && !QUALIFIED_P (name))
    {
      /* 15.13.1: NAME can appear within the scope of a local variable
         declaration */
      if ((decl = IDENTIFIER_LOCAL_VALUE (name)))
        return decl;

      /* 15.13.1: NAME can appear within a class declaration */
      else 
        {
	  decl = lookup_field_wrapper (current_class, name);
	  if (decl)
	    {
	      int fs = FIELD_STATIC (decl);
	      /* Instance variable (8.3.1.1) can't appear within
		 static method, static initializer or initializer for
		 a static variable. */
	      if (!fs && METHOD_STATIC (current_function_decl))
	        {
		  parse_error_context 
		    (id, "Can't make a static reference to nonstatic variable "
		     "`%s' in class `%s'",
		     IDENTIFIER_POINTER (name),
		     IDENTIFIER_POINTER (DECL_NAME 
					 (TYPE_NAME (current_class))));
		  return error_mark_node;
		}
	      decl = build_field_ref ((fs ? NULL_TREE : current_this),
				      current_class, name);
	      return (fs ? build_class_init (current_class, decl) : decl);
	    }
	  /* Fall down to error report on undefined variable */
	}
    }
  /* 6.5.5.2 Qualified Expression Names */
  else
    {
      qualify_ambiguous_name (id);
      /* 15.10.1 Field Access Using a Primary and/or Expression Name */
      /* 15.10.2: Accessing Superclass Members using super */
      return resolve_field_access (id, NULL, NULL);
    }

  /* We've got an error here */
  parse_error_context (id, "Undefined variable `%s'", 
		       IDENTIFIER_POINTER (name));

  return error_mark_node;
}

/* 15.10.1 Field Acess Using a Primary and/or Expression Name.
   We return something suitable to generate the field access. We also
   return the field decl in FIELD_DECL and its type in FIELD_TYPE.  If
   recipient's address can be null. */

static tree
resolve_field_access (qual_wfl, field_decl, field_type)
     tree qual_wfl;
     tree *field_decl, *field_type;
{
  int is_static = 0;
  tree field_ref;
  tree decl, where_found, type_found;

  if (resolve_qualified_expression_name (qual_wfl, &decl,
					 &where_found, &type_found))
    return error_mark_node;

  /* Resolve the LENGTH field of an array here */
  if (DECL_NAME (decl) == length_identifier_node && TYPE_ARRAY_P (type_found)
      && ! flag_emit_class_files)
    {
      tree length = build_java_array_length_access (where_found);
      field_ref =
	build_java_arraynull_check (type_found, length, int_type_node);
    }
  /* We might have been trying to resolve field.method(). In which
     case, the resolution is over and decl is the answer */
  else if (DECL_P (decl) && IDENTIFIER_LOCAL_VALUE (DECL_NAME (decl)) == decl)
    field_ref = decl;
  else if (DECL_P (decl))
    {
      is_static = DECL_P (decl) && FIELD_STATIC (decl);
      field_ref = build_field_ref ((is_static ? NULL_TREE : where_found), 
				   type_found, DECL_NAME (decl));
      if (field_ref == error_mark_node)
	return error_mark_node;
      if (is_static)
	{
	  field_ref = build_class_init (type_found, field_ref);
	  /* If the static field was identified by an expression that
	     needs to be generated, make the field access a compound
	     expression whose first part of the evaluation of the
	     field selector part. */
	  if (where_found && TREE_CODE (where_found) != TYPE_DECL)
	    {
	      tree type = QUAL_DECL_TYPE (field_ref);
	      field_ref = build (COMPOUND_EXPR, type, where_found, field_ref);
	    }
	}
    }
  else
    field_ref = decl;

  if (field_decl)
    *field_decl = decl;
  if (field_type)
    *field_type = QUAL_DECL_TYPE (decl);
  return field_ref;
}

/* 6.5.5.2: Qualified Expression Names */

static int
resolve_qualified_expression_name (wfl, found_decl, where_found, type_found)
     tree wfl;
     tree *found_decl, *type_found, *where_found;
{
  int from_type = 0;		/* Field search initiated from a type */
  int from_super = 0, from_cast = 0;
  int previous_call_static = 0;
  int is_static;
  tree decl = NULL_TREE, type = NULL_TREE, q;
  *where_found = NULL_TREE;

  for (q = EXPR_WFL_QUALIFICATION (wfl); q; q = TREE_CHAIN (q))
    {
      tree qual_wfl = QUAL_WFL (q);

      /* 15.10.1 Field Access Using a Primary */
      
      switch (TREE_CODE (qual_wfl))
	{
	case CALL_EXPR:
	case JAVA_NEW_CLASS_EXPR:
	  /* If the access to the function call is a non static field,
	     build the code to access it. */
	  if (DECL_P (decl) && !FIELD_STATIC (decl))
	    {
	      decl = maybe_access_field (decl, *where_found, type);
	      if (decl == error_mark_node)
		return 1;
	    }
	  /* And code for the function call */
	  if (complete_function_arguments (qual_wfl))
	    return 1;
	  *where_found = 
	    patch_method_invocation_stmt (qual_wfl, decl, type, &is_static);
	  if (*where_found == error_mark_node)
	    return 1;
	  *type_found = type = QUAL_DECL_TYPE (*where_found);

	  /* If the previous call was static and this one is too,
	     build a compound expression to hold the two (because in
	     that case, previous function calls aren't transported as
	     forcoming function's argument. */
	  if (previous_call_static && is_static)
	    {
	      decl = build (COMPOUND_EXPR, type, decl, *where_found);
	      TREE_SIDE_EFFECTS (decl) = 1;
	    }
	  else
	    {
	      previous_call_static = is_static;
	      decl = *where_found;
	    }
	  continue;

	case CONVERT_EXPR:
	  *where_found = decl = java_complete_tree (qual_wfl);
	  if (decl == error_mark_node)
	    return 1;
	  *type_found = type = QUAL_DECL_TYPE (decl);
	  from_cast = 1;
	  continue;

	case ARRAY_REF:
	  /* If the access to the function call is a non static field,
	     build the code to access it. */
	  if (DECL_P (decl) && !FIELD_STATIC (decl))
	    {
	      decl = maybe_access_field (decl, *where_found, type);
	      if (decl == error_mark_node)
		return 1;
	    }
	  /* And code for the array reference expression */
	  decl = java_complete_tree (qual_wfl);
	  if (decl == error_mark_node)
	    return 1;
	  type = QUAL_DECL_TYPE (decl);
	  continue;
	}

      /* If we fall here, we weren't processing a (static) function call. */
      previous_call_static = 0;

      /* It can be the keyword THIS */
      if (EXPR_WFL_NODE (qual_wfl) == this_identifier_node)
	{
	  if (!current_this)
	    {
	      parse_error_context 
		(wfl, "Keyword `this' used outside allowed context");
	      return 1;
	    }
	  /* We have to generate code for intermediate acess */
	  *where_found = decl = current_this;
	  type = QUAL_DECL_TYPE (decl);
	  continue;
	}

      /* 15.10.2 Accessing Superclass Members using SUPER */
      if (EXPR_WFL_NODE (qual_wfl) == super_identifier_node)
	{
	  tree node;
	  /* Check on the restricted use of SUPER */
	  if (METHOD_STATIC (current_function_decl)
	      || current_class == object_type_node)
	    {
	      parse_error_context 
		(wfl, "Keyword `super' used outside allowed context");
	      return 1;
	    }
	  /* Otherwise, treat SUPER as (SUPER_CLASS)THIS */
	  node = build_cast (EXPR_WFL_LINECOL (qual_wfl), 
			     CLASSTYPE_SUPER (current_class),
			     build_this (EXPR_WFL_LINECOL (qual_wfl)));
	  *where_found = decl = java_complete_tree (node);
	  *type_found = type = QUAL_DECL_TYPE (decl);
	  from_super = from_type = 1;
	  continue;
	}

      /* 15.13.1: Can't search for field name in packages, so we
	 assume a variable/class name was meant. */
      if (RESOLVE_PACKAGE_NAME_P (qual_wfl))
	{
	  if (from_super || from_cast)
	    parse_error_context 
	      ((from_cast ? qual_wfl : wfl),
	       "No variable `%s' defined in class `%s'",
	       IDENTIFIER_POINTER (EXPR_WFL_NODE (qual_wfl)),
	       lang_printable_name (type));
	  else
	    parse_error_context
	      (qual_wfl, "Undefined variable or class name: `%s'",
	       IDENTIFIER_POINTER (EXPR_WFL_NODE (qual_wfl)));
	  return 1;
	}

      /* We have a type name. It's been already resolved when the
	 expression was qualified. */
      else if (RESOLVE_TYPE_NAME_P (qual_wfl))
	{
	  if (!(decl = QUAL_RESOLUTION (q)))
	    return 1;		/* Error reported already */

	  if (not_accessible_p (TREE_TYPE (decl), decl, 0))
	    {
	      parse_error_context 
		(qual_wfl, "Can't access %s field `%s.%s' from `%s'",
		 java_accstring_lookup (get_access_flags_from_decl (decl)),
		 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))),
		 IDENTIFIER_POINTER (DECL_NAME (decl)),
		 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))));
	      return 1;
	    }
	  
	  type = TREE_TYPE (decl);
	  from_type = 1;
	}
      /* We resolve and expression name */
      else 
	{
	  tree field_decl;

	  /* If there exists an early resolution, use it. That occurs
	     only once and we know that there are more things to
	     come. Don't do that when processing something after SUPER
	     (we need more thing to be put in place below */
	  if (!from_super && QUAL_RESOLUTION (q))
	    decl = QUAL_RESOLUTION (q);

	  /* We have to search for a field, knowing the type of its
             container. The flag FROM_TYPE indicates that we resolved
             the last member of the expression as a type name, which
             means that for the resolution of this field, will check
             on other errors than if the it was resolved as a member
             of an other field. */
	  else
	    {
	      int is_static;
	      if (!from_type && !JREFERENCE_TYPE_P (type))
		{
		  parse_error_context 
		    (qual_wfl, "Attempt to reference field `%s' in `%s %s'",
		     IDENTIFIER_POINTER (EXPR_WFL_NODE (qual_wfl)),
		     lang_printable_name (type),
		     IDENTIFIER_POINTER (DECL_NAME (field_decl)));
		  return 1;
		}
	      
	      if (!(field_decl = 
		    lookup_field_wrapper (type, EXPR_WFL_NODE (qual_wfl))))
		{
		  parse_error_context 
		    (qual_wfl, "No variable `%s' defined in class `%s'",
		     IDENTIFIER_POINTER (EXPR_WFL_NODE (qual_wfl)), 
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
		  return 1;
		}
	      
	      /* Check on accessibility here */
	      if (not_accessible_p (type, field_decl, from_super))
		{
		  parse_error_context 
		    (qual_wfl,
		     "Can't access %s field `%s.%s' from `%s'",
		     java_accstring_lookup 
		       (get_access_flags_from_decl (field_decl)),
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))),
		     IDENTIFIER_POINTER (DECL_NAME (field_decl)),
		     IDENTIFIER_POINTER 
		       (DECL_NAME (TYPE_NAME (current_class))));
		  return 1;
		}
	      
	      /* There are things to check when fields are accessed
	         from type. There are no restrictions on a static
	         declaration of the field when it is accessed from an
	         interface */
	      is_static = FIELD_STATIC (field_decl);
	      if (!from_super && from_type 
		  && !TYPE_INTERFACE_P (type) && !is_static)
		{
		  parse_error_context 
		    (qual_wfl, "Can't make a static reference to nonstatic "
		     "variable `%s' in class `%s'",
		     IDENTIFIER_POINTER (EXPR_WFL_NODE (qual_wfl)),
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
		  return 1;
		}
	      from_cast = from_super = 0;

	      /* If we need to generate something to get a proper handle
		 on what this field is accessed from, do it now. */
	      if (!is_static)
		{
		  decl = maybe_access_field (decl, *where_found, type);
		  if (decl == error_mark_node)
		    return 1;
		}

	      /* We want to keep the location were found it, and the type
		 we found. */
	      *where_found = decl;
	      *type_found = type;

	      /* This is the decl found and eventually the next one to
		 search from */
	      decl = field_decl;
	    }

	  from_type = 0;
	  type = QUAL_DECL_TYPE (decl);
	}
    }
  *found_decl = decl;
  return 0;
}

/* 6.6 Qualified name and access control. Returns 1 if MEMBER (a decl)
   can't be accessed from REFERENCE (a record type). */

int not_accessible_p (reference, member, from_super)
     tree reference, member;
     int from_super;
{
  int access_flag = get_access_flags_from_decl (member);

  /* Access always granted for members declared public */
  if (access_flag & ACC_PUBLIC)
    return 0;
  
  /* Check access on protected members */
  if (access_flag & ACC_PROTECTED)
    {
      /* Access granted if it occurs from within the package
         containing the class in which the protected member is
         declared */
      if (class_in_current_package (DECL_CONTEXT (member)))
	return 0;

      if (TREE_CODE (member) == FUNCTION_DECL && DECL_CONSTRUCTOR_P (member))
	{
	  /* Access from SUPER is granted */
	  if (from_super)
	    return 0;
	  /* Otherwise, access isn't granted */
	  return 1;
	}
      else
	{
	  /* If accessed with the form `super.member', then access is
             granted */
	  if (from_super)
	    return 0;

	  /* Otherwise, access is granted if occuring from the class where
	     member is declared or a subclass of it */
	  if (inherits_from_p (reference, current_class))
	    return 0;
	}
      return 1;
    }

  /* Check access on private members. Access is granted only if it
     occurs from within the class in witch it is declared*/

  if (access_flag & ACC_PRIVATE)
    return (current_class == DECL_CONTEXT (member) ? 0 : 1);

  /* Default access are permitted only when occuring within the
     package in which the type (REFERENCE) is declared. In other words,
     REFERENCE is defined in the current package */
  if (ctxp->package)
    return !class_in_current_package (reference);
  
  /* Otherwise, access is granted */
  return 0;
}

/* Returns 1 if class was declared in the current package, 0 otherwise */

static int
class_in_current_package (class)
     tree class;
{
  static tree cache = NULL_TREE;
  int qualified_flag;
  tree left;

  if (cache == class)
    return 1;

  qualified_flag = QUALIFIED_P (DECL_NAME (TYPE_NAME (class)));

  /* If the current package is empty and the name of CLASS is
     qualified, class isn't in the current package.  If there is a
     current package and the name of the CLASS is not qualified, class
     isn't in the current package */
  if (!ctxp->package && qualified_flag || ctxp->package && !qualified_flag)
    return 0;

  /* If there is not package and the name of CLASS isn't qualified,
     they belong to the same unnamed package */
  if (!ctxp->package && !qualified_flag)
    return 1;

  /* Compare the left part of the name of CLASS with the package name */
  breakdown_qualified (&left, NULL, DECL_NAME (TYPE_NAME (class)));
  if (ctxp->package == left)
    {
      cache = class;
      return 1;
    }
  return 0;
}

/* This function may generate code to access DECL from WHERE. This is
   done only if certain conditions meet.  */

static tree
maybe_access_field (decl, where, type)
  tree decl, where, type;
{
  if (DECL_P (decl) && decl != current_this
      && (!(TREE_CODE (decl) != PARM_DECL
	    && FIELD_STATIC (decl)))
      && !IDENTIFIER_LOCAL_VALUE (DECL_NAME (decl)))
    decl = build_field_ref (where ? where : current_this, 
			    type, DECL_NAME (decl));
  return decl;
}

/* Build a method invocation statement, by patching PATCH. If non NULL
   and according to the situation, PRIMARY and WHERE may be
   used. IS_STATIC is set to 1 if the invoked function is static. */

static tree
patch_method_invocation_stmt (patch, primary, where, is_static)
     tree patch, primary, where;
     int *is_static;
{
  tree wfl = TREE_OPERAND (patch, 0);
  tree args = TREE_OPERAND (patch, 1);
  tree name = EXPR_WFL_NODE (wfl);
  tree list, class_type;
  
  /* Should be overriden if everything goes well. Otherwise, if
     something fails, it should keep this value. It stop the
     evaluation of a bogus assignment. See java_complete_tree,
     MODIFY_EXPR: for the reasons why we sometimes want to keep on
     evaluating an assignment */
  TREE_TYPE (patch) = error_mark_node;

  /* Since lookup functions are messing with line numbers, save the
     context now.  */
  java_parser_context_save_global ();

  /* 15.11.1: Compile-Time Step 1: Determine Class or Interface to Search */

  /* Resolution of qualified name, excluding constructors */
  if (QUALIFIED_P (name) && !CALL_CONSTRUCTOR_P (patch))
    {
      tree class_decl, identifier, identifier_wfl;
      /* Extract the last IDENTIFIER of the qualified
	 expression. This is a wfl and we will use it's location
	 data during error report. */
      identifier_wfl = cut_identifier_in_qualified (wfl);
      identifier = EXPR_WFL_NODE (identifier_wfl);
      
      /* Given the context, IDENTIFIER is syntactically qualified
	 as a MethodName. We need to qualify what's before */
      qualify_ambiguous_name (wfl);

      /* Package resolution are erroneous */
      if (RESOLVE_PACKAGE_NAME_P (wfl))
	{
	  tree remainder;
	  breakdown_qualified (&remainder, NULL, EXPR_WFL_NODE (wfl));
	  parse_error_context (wfl, "Can't search method `%s' in package "
			       "`%s'",IDENTIFIER_POINTER (identifier),
			       IDENTIFIER_POINTER (remainder));
	  return error_mark_node;
	}
      /* We're resolving a call from a type */
      else if (RESOLVE_TYPE_NAME_P (wfl))
	{
	  tree decl = QUAL_RESOLUTION (EXPR_WFL_QUALIFICATION (wfl));
	  tree name = DECL_NAME (decl);
	  tree type;

	  class_decl = resolve_and_layout (name, wfl);
	  if (CLASS_INTERFACE (decl))
	    {
	      parse_error_context
		(identifier_wfl, "Can't make static reference to method "
		 "`%s' in interface `%s'", IDENTIFIER_POINTER (identifier), 
		 IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  /* Look the method up in the type selector. The method ought
             to be static. */
	  type = TREE_TYPE (class_decl);
	  list = lookup_method_invoke (0, wfl, type, identifier, args);
	  if (list && !METHOD_STATIC (list))
	    {
	      char *fct_name = strdup ((char *)lang_printable_name (list));
	      parse_error_context 
		(identifier_wfl,
		 "Can't make static reference to method `%s %s' in class `%s'",
		 lang_printable_name (TREE_TYPE (TREE_TYPE (list))), fct_name, 
		 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
	      free (fct_name);
	      return error_mark_node;
	    }
	}
      /* We're resolving an expression name */
      else
	{
	  tree field, type;
	  
	  /* 1- Find the field to which the call applies */
	  field = resolve_field_access (wfl, NULL, &type);
	  if (field == error_mark_node)
	    return error_mark_node;
	  
	  /* 2- Do the layout of the class where the last field
	     was found, so we can search it. */
	  class_decl = 
	    resolve_and_layout (DECL_NAME (TYPE_NAME (type)), NULL_TREE);
	  
	  /* 3- Retrieve a filtered list of method matches, Refine
	     if necessary. In any cases, point out errors.  */
	  list = lookup_method_invoke (0, identifier_wfl, type, 
				       identifier, args);

	  /* 4- Add the field as an argument */
	  args = tree_cons (NULL_TREE, field, args);
	}

      /* CLASS_TYPE is used during the call to not_accessible_p and
	 IDENTIFIER_WFL will be used to report any problem further */
      class_type = TREE_TYPE (class_decl);
      wfl = identifier_wfl;
    }
  /* Resolution of simple names, names generated after a primary: or
     constructors */
  else
    {
      tree class_to_search;
      int lc;		/* Looking for Constructor */
      
      /* We search constructor in their target class */
      if (CALL_CONSTRUCTOR_P (patch))
	{
	  class_to_search = resolve_no_layout (EXPR_WFL_NODE (wfl), NULL_TREE);
	  if (!class_to_search)
	    {
	      parse_error_context 
		(wfl, "Class `%s' not found in type declaration",
		 IDENTIFIER_POINTER (EXPR_WFL_NODE (wfl)));
	      return error_mark_node;
	    }
	  
	  /* Can't instantiate an abstract class */
	  if (CLASS_ABSTRACT (class_to_search))
	    {
	      parse_error_context 
		(wfl, "Class `%s' is an abstract class. It can't be "
		 "instantiated", IDENTIFIER_POINTER (EXPR_WFL_NODE (wfl)));
	      return error_mark_node;
	    }
	  class_to_search = TREE_TYPE (class_to_search);
	  lc = 1;
	}
      /* This is a regular search in the local class, unless an
         alternate class is specified. */
      else
	{
	  class_to_search = (where ? where : current_class);
	  lc = 0;
	}
      
      /* NAME is a simple identifier or comes from a primary. Search
	 in the class whose declaration contain the method being
	 invoked. */
      list = lookup_method_invoke (lc, wfl, class_to_search, name, args);

      /* Don't continue if no method were found, as the next statement
         can't be executed then. */
      if (!list) return error_mark_node;

      /* Check for static reference if non static methods */
      if (check_for_static_method_reference (wfl, patch, list, 
					     class_to_search, primary))
	return error_mark_node;

      /* Non static/constructor methods are called with the current
	 object extra argument. If method is resolved as a primary,
	 use the primary otherwise use the current THIS. */
      if (!CALL_CONSTRUCTOR_P (patch) && !METHOD_STATIC (list))
	args = tree_cons (NULL_TREE, primary ? primary : current_this, args);

      class_type = class_to_search;
    }
  
  /* Merge point of all resolution schemes. If we have nothing, this
     is an error, already signaled */
  if (!list) return error_mark_node;
  
  /* Check accessibility, position the is_static flag, build and
     return the call */
  if (not_accessible_p (class_type, list, 0))
    {
      char *fct_name = strdup ((char *)lang_printable_name (list));
      parse_error_context 
	(wfl, "Can't access %s method `%s %s.%s' from `%s'",
	 java_accstring_lookup (get_access_flags_from_decl (list)),
	 lang_printable_name (TREE_TYPE (TREE_TYPE (list))), 
	 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (class_type))), fct_name,
	 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))));
      free (fct_name);
      return error_mark_node;
    }
  
  if (is_static) 
    *is_static = METHOD_STATIC (list);
  java_parser_context_restore_global ();
  return patch_invoke (patch, list, args, wfl);
}

/* Check that we're not trying to do a static reference to a method in
   non static method. Return 1 if it's the case, 0 otherwise. */

static int
check_for_static_method_reference (wfl, node, method, where, primary)
     tree wfl, node, method, where, primary;
{
  if (METHOD_STATIC (current_function_decl) 
      && !METHOD_STATIC (method) && !primary && !CALL_CONSTRUCTOR_P (node))
    {
      char *fct_name = strdup ((char *)lang_printable_name (method));
      parse_error_context 
	(wfl, "Can't make static reference to method `%s %s' in class `%s'", 
	 lang_printable_name (TREE_TYPE (TREE_TYPE (method))), fct_name,
	 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (where))));
      free (fct_name);
      return 1;
    }
  return 0;
}

/* Patch an invoke expression METHOD and ARGS, based on its invocation
   mode.  */

static tree
patch_invoke (patch, method, args, cl)
     tree patch, method, args;
     tree cl;
{
  tree dtable, func;
  tree signature = build_java_signature (TREE_TYPE (method));
  tree original_call;

  switch (invocation_mode (method, 0))
    {
    case INVOKE_VIRTUAL:
      dtable = invoke_build_dtable (0, args);
      func = build_invokevirtual (dtable, method);
      break;
    case INVOKE_STATIC:
      func = build_known_method_ref (method, TREE_TYPE (method),
				     DECL_CONTEXT (method),
				     signature, args);
      args = nreverse (args);
      break;

    default:
      fatal ("Unknown invocation mode - build_invoke");
      return NULL_TREE;
    }


  /* Ensure self_type is initialized, (invokestatic). FIXME */
  func = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (method)), func);
  TREE_TYPE (patch) = TREE_TYPE (TREE_TYPE (method));
  TREE_OPERAND (patch, 0) = func;
  TREE_OPERAND (patch, 1) = args;
  original_call = patch;

  /* We're calling a constructor. New is called an its returned value
     is an argument to the constructor. We build a COMPOUND_EXPR and
     use saved expression so that the overall NEW expression value is
     a pointer to a newly created and initialized class. */
  if (CALL_CONSTRUCTOR_P (original_call))
    {
      tree class = DECL_CONTEXT (method);
      tree c1, saved_new, size, new;
      if (!TYPE_SIZE (class))
	safe_layout_class (class);
      size = size_in_bytes (class);
      new = build (CALL_EXPR, promote_type (class),
		   build_address_of (alloc_object_node),
		   tree_cons (NULL_TREE, build_class_ref (class),
			      build_tree_list (NULL_TREE, 
					       size_in_bytes (class))),
		   NULL_TREE);
      saved_new = save_expr (new);
      c1 = build_tree_list (NULL_TREE, saved_new);
      TREE_CHAIN (c1) = TREE_OPERAND (original_call, 1);
      TREE_OPERAND (original_call, 1) = c1;
      TREE_SET_CODE (original_call, CALL_EXPR);
      patch = build (COMPOUND_EXPR, TREE_TYPE (new), patch, saved_new);
    }
  return patch;
}

static int
invocation_mode (method, super)
     tree method;
     int super;
{
  int access = get_access_flags_from_decl (method);

  if (access & ACC_STATIC)
    return INVOKE_STATIC;

  if (CLASS_FINAL (TYPE_NAME (DECL_CONTEXT (method))))
    return INVOKE_STATIC;
  
  if (super)
    return INVOKE_SUPER;
  
  if (CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (method))))
    return INVOKE_INTERFACE;
  
  if (DECL_CONSTRUCTOR_P (method))
    return INVOKE_STATIC;
  
  return INVOKE_VIRTUAL;
}

/* Retrieve a refined list of matching methods. */

static tree
lookup_method_invoke (lc, cl, class, name, arg_list)
     int lc;
     tree cl;
     tree class, name, arg_list;
{
  tree method = make_node (FUNCTION_TYPE);
  tree arg_type_list = NULL_TREE;
  tree signature, list, node, scratch;

  for (node = arg_list; node; node = TREE_CHAIN (node))
    {
      tree current_arg;
      current_arg = 
	build_tree_list (NULL_TREE,
			 promote_type (TREE_TYPE (TREE_VALUE (node))));
      arg_type_list = chainon (current_arg, arg_type_list);
    }
  TYPE_ARG_TYPES (method) = arg_type_list;

  if (!lc)
    {
      signature = build_java_argument_signature (method);
      list = match_java_method (class, name, signature);
      list = refine_accessible_methods_list (lc, list);
    }
  else
    {
      TREE_TYPE (method) = void_type_node;
      signature = build_java_signature (method);
      list = lookup_java_constructor (class, signature);
    }

  if (!list)
    {
      parse_error_context (cl, "Can't find method `%s(%s)' in class `%s'",
			   IDENTIFIER_POINTER (name),
			   IDENTIFIER_POINTER (signature),
			   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (class))));
      return NULL_TREE;
    }

  if (lc)
    return list;

  if (TREE_CHAIN (list))
    {
      tree most_specific_list = NULL_TREE;
      tree current;
      /* 15.11.2.2 Choose the Most Specific Method */
      for (current = list; current; current = TREE_CHAIN (current))
	{
	  tree rest;
	  tree method = TREE_VALUE (list);
	  tree class_from = DECL_CONTEXT (method);
	  for (rest = TREE_CHAIN (current); rest; rest = TREE_CHAIN (rest))
	    {
	      tree other = TREE_VALUE (rest);

	      /* METHOD can be declared more specific with regard to OTHER iif:
		
		 - The class METHOD belongs can be converted to the
		   class OTHER belongs by method invocation conversion
		   (5.3).  Since we're dealing with classes here, it is
		   covered by the identity conversion or the windening
		   primitive conversion.
		
		 - The types of the arguments of METHOD can be
		   converted to the types of the arguments of OTHER by
		   method invocation conversion (5.3). */

	      if (valid_ref_assignconv_cast_p (class_from, 
					       DECL_CONTEXT (other), 0)
		  && 1)		/* Test on args non implemented */
		most_specific_list = tree_cons (NULL_TREE, method, 
						most_specific_list);
	    }
	}
      list = most_specific_list;
    }

  if (!list || TREE_CHAIN (list))
    {
      parse_error_context (cl, "Can't find method `%s(%s)' in class `%s'",
			   IDENTIFIER_POINTER (name),
			   IDENTIFIER_POINTER (signature),
			   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (class))));
      return NULL_TREE;
    }

  /* 15.11.3 Is the Chosen Method Appropriate ? */
  else
    return TREE_VALUE (list);
}

/* Refine accessible methods from the raw matching method list, as
   specified in 15.11.4.3. Return a (possibly empty) new method
   list.  */

static tree
refine_accessible_methods_list (lc, list)
     int lc;			/* Looking for Constructor */
     tree list;
{
#define ADD_TO_LIST_AND_CONTINUE				\
  {								\
    refined_list = tree_cons (NULL_TREE, method, refined_list);	\
    continue;							\
  }
  tree node, refined_list = NULL_TREE;
  tree current_class_name = DECL_NAME (TYPE_NAME (current_class));

  for (node = list; node; node = TREE_CHAIN (node))
    {
      int access, identical;
      tree class_from, method, class_from_name;
      
      method = TREE_VALUE (node);

      /* Constructor not retained here, unless were specifically
       looking for them. */
      if (lc && DECL_CONSTRUCTOR_P (method))
	ADD_TO_LIST_AND_CONTINUE;

      access = get_access_flags_from_decl (method);
      class_from = DECL_CONTEXT (method);
      class_from_name = DECL_NAME (TYPE_NAME (class_from));
      
      identical = identical_subpath_p (current_class_name, class_from_name);

      /* Check accessibility of class_from from the current one: This
	 test has been already carried out when qualify_ambiguous_name
	 tried to resolve a type found in an other package. It is not
	 necessary to retest things here, the error has been already
	 reported. */
		  
      /* Public method are always OK */
      if (access & ACC_PUBLIC)
	ADD_TO_LIST_AND_CONTINUE;
      
      /* Protected method access is OK if classes are from the
	 same package or part of the same inheritance lineage */
      if ((access & ACC_PROTECTED)
	  && (inherits_from_p (current_class, class_from) || identical))
	ADD_TO_LIST_AND_CONTINUE;

      /* Methods with default (package) access are OK if classes are
	 from the same default package. */
      if (identical || 
	  (!QUALIFIED_P (class_from_name) && !QUALIFIED_P (current_class_name)))
	ADD_TO_LIST_AND_CONTINUE;

      /* Private method accessible iff current class is the node where
	 the method is defined */
      if ((access & ACC_PRIVATE) && (class_from == current_class))
	ADD_TO_LIST_AND_CONTINUE;
    }
#undef ADD_TO_LIST_AND_CONTINUE
  return refined_list;
}

/* Qualification routines */

static void
qualify_ambiguous_name (id)
     tree id;
{
  tree qual, qual_wfl, name, decl, ptr_type, saved_current_class;
  int again, super_found = 0, this_found = 0;

  /* We first qualify the first element, then derive qualification of
     others based on the first one. If the first element is qualified
     by a resolution (field or type), this resolution is stored in the
     QUAL_RESOLUTION of the qual element being examined. We need to
     save the current_class since the use of SUPER might change the
     its value. */
  saved_current_class = current_class;
  qual = EXPR_WFL_QUALIFICATION (id);
  do {

    /* Simple qualified expression feature a qual_wfl that is a
       WFL. Expression derived from a primary feature more complicated
       things like a CALL_EXPR. Expression from primary need to be
       worked out to extract the part on which the qualification will
       take place. */
    qual_wfl = QUAL_WFL (qual);
    switch (TREE_CODE (qual_wfl))
      {
      case CALL_EXPR:
	qual_wfl = TREE_OPERAND (qual_wfl, 0);
	if (TREE_CODE (qual_wfl) != EXPR_WITH_FILE_LOCATION)
	  {
	    qual = EXPR_WFL_QUALIFICATION (qual_wfl);
	    qual_wfl = QUAL_WFL (qual);
	  }
	break;
      case JAVA_NEW_CLASS_EXPR:
      case CONVERT_EXPR:
      case ARRAY_REF:
	qual_wfl = TREE_OPERAND (qual_wfl, 0);
	break;
      }
    name = EXPR_WFL_NODE (qual_wfl);
    ptr_type = current_class;
    again = 0;
    /* If we have a THIS (from a primary), we set the context accordingly */
    if (name == this_identifier_node)
      {
	qual = TREE_CHAIN (qual);
	qual_wfl = QUAL_WFL (qual);
	name = EXPR_WFL_NODE (qual_wfl);
	this_found = 1;
      }
    /* If we have a SUPER, we set the context accordingly */
    if (name == super_identifier_node)
      {
	current_class = CLASSTYPE_SUPER (ptr_type);
	/* Check that there is such a thing as a super class. If not,
	   return.  The error will be caught later on, during the
	   resolution */
	if (!current_class)
	  {
	    current_class = saved_current_class;
	    return;
	  }
	qual = TREE_CHAIN (qual);
	/* Do one more interation to set things up */
	super_found = again = 1;
      }
  } while (again);
  
  /* If name appears within the scope of a location variable
     declaration or parameter declaration, then it is an expression
     name. We don't carry this test out if we're in the context of the
     use of SUPER or THIS */

  if (!this_found && !super_found && (decl = IDENTIFIER_LOCAL_VALUE (name)))
    {
      RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = decl;
    }

  /* If within the class/interface NAME was found to be used there
     exists a (possibly inherited) field named NAME, then this is an
     expression name. */
  else if ((decl = lookup_field_wrapper (ptr_type, name)))
    {
      RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = decl;
    }

  /* We reclassify NAME as a type name if:
     - NAME is a class/interface declared within the compilation
       unit containing NAME,
     - NAME is imported via a single-type-import declaration,
     - NAME is declared in an another compilation unit of the package
       of the compilation unit containing NAME,
     - NAME is declared by exactly on type-import-on-demand declaration
     of the compilation unit containing NAME. */
  else if ((decl = resolve_and_layout (name, NULL_TREE)))
    {
      RESOLVE_TYPE_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = decl;
    }

  /* Method call are expression name */
  else if (TREE_CODE (QUAL_WFL (qual)) == CALL_EXPR)
    RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;

  /* Check here that NAME isn't declared by more than one
     type-import-on-demand declaration of the compilation unit
     containing NAME. FIXME */

  /* Otherwise, NAME is reclassified as a package name */
  else 
    RESOLVE_PACKAGE_NAME_P (qual_wfl) = 1;

  /* Propagate the qualification accross other components of the
     qualified name */
  for (qual = TREE_CHAIN (qual); qual;
       qual_wfl = QUAL_WFL (qual), qual = TREE_CHAIN (qual))
    {
      if (RESOLVE_PACKAGE_NAME_P (qual_wfl))
	RESOLVE_PACKAGE_NAME_P (QUAL_WFL (qual)) = 1;
      else 
	RESOLVE_EXPRESSION_NAME_P (QUAL_WFL (qual)) = 1;
    }

  /* Store the global qualification for the ambiguous part of ID back
     into ID fields */
  if (RESOLVE_EXPRESSION_NAME_P (qual_wfl))
    RESOLVE_EXPRESSION_NAME_P (id) = 1;
  else if (RESOLVE_TYPE_NAME_P (qual_wfl))
    RESOLVE_TYPE_NAME_P (id) = 1;
  else if (RESOLVE_PACKAGE_NAME_P (qual_wfl))
    RESOLVE_PACKAGE_NAME_P (id) = 1;

  /* Restore the current class */
  current_class = saved_current_class;
}

static int
breakdown_qualified (left, right, source)
    tree *left, *right, source;
{
  char *p = IDENTIFIER_POINTER (source), *base;
  int   l = IDENTIFIER_LENGTH (source);

  /* Breakdown NAME into REMAINDER . IDENTIFIER */
  base = p;
  p += (l-1);
  while (*p != '.' && p != base)
    p--;

  /* We didn't find a '.'. Return an error */
  if (p == base)
    return 1;

  *p = '\0';
  if (right)
    *right = get_identifier (p+1);
  *left = get_identifier (IDENTIFIER_POINTER (source));
  *p = '.';
  
  return 0;
}

/* Return 1 if N1 and N2 have identical sub-path. */

static int
identical_subpath_p (n1, n2)
     tree n1, n2;
{
  tree left1, left2;

  if (!QUALIFIED_P (n1) || !QUALIFIED_P (n2))
    return n1 == n2;
  
  breakdown_qualified (&left1, NULL, n1);
  breakdown_qualified (&left2, NULL, n2);

  return left1 == left2;
}

static int
not_initialized_as_it_should_p (decl)
     tree decl;
{
  if (DECL_P (decl))
    {
      if (TREE_CODE (decl) == FIELD_DECL
	  && METHOD_STATIC (current_function_decl))
	return 0;
      return DECL_P (decl) && !INITIALIZED_P (decl);
    }
  return 0;
}

/* Patch tree nodes in a function body. When a BLOCK is found, push
   local variable decls if present.  */

static tree
java_complete_tree (node)
     tree node;
{
  tree nn, cn, wfl_op1, wfl_op2;
  int flag, location;

  /* CONVERT_EXPR always has its type set, even though it needs to be
     worked out */
  if (TREE_TYPE (node) && TREE_CODE (node) != CONVERT_EXPR)
    return node;

  /* The switch block implements cases processing container nodes
     first.  Contained nodes are always written back. Leaves come
     next and return a value. */
  switch (TREE_CODE (node))
    {
    case BLOCK:

      /* 1- Block section.
	 Set the local values on decl names so we can identify them
	 faster when they're referenced. At that stage, identifiers
	 are legal so we don't check for declaration errors. */
      for (cn = BLOCK_EXPR_DECLS (node); cn; cn = TREE_CHAIN (cn))
	{
	  DECL_CONTEXT (cn) = current_function_decl;
	  IDENTIFIER_LOCAL_VALUE (DECL_NAME (cn)) = cn;
	  INITIALIZED_P (cn) = 0;
	}
      if (BLOCK_EXPR_BODY (node))
	{
	  BLOCK_EXPR_BODY (node) = java_complete_tree (BLOCK_EXPR_BODY (node));
	  if (BLOCK_EXPR_BODY (node) == error_mark_node)
	    return error_mark_node;
	}
      /* Turn local bindings to null */
      for (cn = BLOCK_EXPR_DECLS (node); cn; cn = TREE_CHAIN (cn))
	IDENTIFIER_LOCAL_VALUE (DECL_NAME (cn)) = NULL_TREE;

      TREE_TYPE (node) = void_type_node;
      break;

      /* 2- They are expressions but ultimately deal with statements */
    case LABELED_BLOCK_EXPR:
      PUSH_LABELED_BLOCK (node);
      if (LABELED_BLOCK_BODY (node))
	COMPLETE_CHECK_OP_1 (node);
      TREE_TYPE (node) = void_type_node;
      POP_LABELED_BLOCK ();
      return node;

    case EXIT_BLOCK_EXPR:
      /* We don't complete operand 1, because it's the return value of
         the EXIT_BLOCK_EXPR which doesn't exist it Java */
      return patch_bc_statement (node);

    case LOOP_EXPR:
      PUSH_LOOP (node);
      /* Check whether the loop was enclosed in a labeled
         statement. If not, create one, insert the loop in it and
         return the node */
      nn = patch_loop_statement (node);
      /* Anyways, walk the body of the loop */
      TREE_OPERAND (node, 0) = java_complete_tree (TREE_OPERAND (node, 0));
      if (TREE_OPERAND (node, 0) == error_mark_node)
	return error_mark_node;
      TREE_TYPE (nn) = TREE_TYPE (node) = void_type_node;
      /* If we returned something different, that's because we
         inserted a label. Pop the label too. */
      if (nn != node)
	POP_LABELED_BLOCK ();
      POP_LOOP ();
      return nn;

    case EXIT_EXPR:
      TREE_OPERAND (node, 0) = java_complete_tree (TREE_OPERAND (node, 0));
      return patch_exit_expr (node);

    case COND_EXPR:
      /* Condition */
      TREE_OPERAND (node, 0) = java_complete_tree (TREE_OPERAND (node, 0));
      if (TREE_OPERAND (node, 0) == error_mark_node)
	return error_mark_node;
      /* then-else branches */
      TREE_OPERAND (node, 1) = java_complete_tree (TREE_OPERAND (node, 1));
      if (TREE_OPERAND (node, 1) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (node, 2) = java_complete_tree (TREE_OPERAND (node, 2));
      if (TREE_OPERAND (node, 2) == error_mark_node)
	return error_mark_node;
      return patch_if_else_statement (node);
      break;

      /* 3- Expression section */
    case COMPOUND_EXPR:
      TREE_OPERAND (node, 0) = java_complete_tree (TREE_OPERAND (node, 0));
      TREE_OPERAND (node, 1) = java_complete_tree (TREE_OPERAND (node, 1));
      if (TREE_OPERAND (node, 1) == error_mark_node)
	return error_mark_node;
      TREE_TYPE (node) = TREE_TYPE (TREE_OPERAND (node, 1));
      break;

    case RETURN_EXPR:
      return patch_return (node);

    case EXPR_WITH_FILE_LOCATION:
      if (!EXPR_WFL_NODE (node) /* Or a PRIMARY flag ? */
	  || TREE_CODE (EXPR_WFL_NODE (node)) == IDENTIFIER_NODE)
        return resolve_expression_name (node);
      else
	{
	  EXPR_WFL_NODE (node) = java_complete_tree (EXPR_WFL_NODE (node));
	  TREE_SIDE_EFFECTS (node) = 1;
	  if (EXPR_WFL_NODE (node) == error_mark_node)
	    {
	      /* Its important for the evaluation of assignment that
		 this mark on the TREE_TYPE is propagated. */
	      TREE_TYPE (node) = error_mark_node;
	      return error_mark_node;
	    }
	  else
	    TREE_TYPE (node) = TREE_TYPE (EXPR_WFL_NODE (node));
	}
      break;

    case JAVA_NEW_ARRAY_EXPR:
      /* Patch all the dimensions */
      flag = 0;
      for (cn = TREE_OPERAND (node, 1); cn; cn = TREE_CHAIN (cn))
	{
	  int location = EXPR_WFL_LINECOL (TREE_VALUE (cn));
	  tree dim = java_complete_tree (TREE_VALUE (cn));
	  if (dim == error_mark_node)
	    {
	      flag = 1;
	      continue;
	    }
	  else
	    {
	      TREE_VALUE (cn) = save_expr (dim);
	      /* Setup the location of the current dimension, for
		 later error report. */
	      TREE_PURPOSE (cn) = 
		build_expr_wfl (NULL_TREE, input_filename, 0, 0);
	      EXPR_WFL_LINECOL (TREE_PURPOSE (cn)) = location;
	    }
	}
      /* They complete the array creation expression, if no errors
         were found. */
      return (flag ? error_mark_node : patch_newarray (node));

    case JAVA_NEW_CLASS_EXPR:
    case CALL_EXPR:
      /* Complete function's argument first */
      if (complete_function_arguments (node))
	return error_mark_node;
      else
	return patch_method_invocation_stmt (node, NULL_TREE, NULL_TREE, NULL);

    case MODIFY_EXPR:
      /* Save potential wfls */
      wfl_op1 = TREE_OPERAND (node, 0);
      wfl_op2 = TREE_OPERAND (node, 1);
      TREE_OPERAND (node, 0) = java_complete_tree (wfl_op1);
      if (TREE_OPERAND (node, 0) == error_mark_node)
	return error_mark_node;

      if (COMPOUND_ASSIGN_P (wfl_op2))
	{
	  tree lvalue;
	  tree other = 
	    java_complete_tree (TREE_OPERAND (wfl_op2, 0));

	  /* Hand stablize the lhs on both places */
	  lvalue = stabilize_reference (other); 
	  TREE_OPERAND (node, 0) = lvalue;
	  TREE_OPERAND (TREE_OPERAND (node, 1), 0) = lvalue;
	}

      /* There are cases where the type of RHS is fixed. In those
	 cases, if the evaluation of the RHS fails, we further the
	 evaluation of the assignment to detect more errors. */
      nn = java_complete_tree (TREE_OPERAND (node, 1));
      if (nn == error_mark_node)
	{
	  /* It's hopeless, but we can further things on to discover
	     an error during the assignment. In any cases, the
	     assignment operation fails. */
	  if (TREE_CODE (TREE_OPERAND (node, 1)) != EXPR_WITH_FILE_LOCATION
	      && TREE_TYPE (TREE_OPERAND (node, 1)) != error_mark_node)
	    patch_assignment (node, wfl_op1, wfl_op2);

	  /* Now, we still mark the lhs as initialized */
	  if (DECL_P (TREE_OPERAND (node, 0)))
	    INITIALIZED_P (TREE_OPERAND (node, 0)) = 1;

	  return error_mark_node;
	}
      TREE_OPERAND (node, 1) = nn;
      return patch_assignment (node, wfl_op1, wfl_op2);

    case MULT_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case URSHIFT_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case BIT_IOR_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case EQ_EXPR: 
    case NE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      /* Operands 0 and 1 are WFL in certain cases only. patch_binop
	 knows how to handle those cases. */
      wfl_op1 = TREE_OPERAND (node, 0);
      wfl_op2 = TREE_OPERAND (node, 1);
      TREE_OPERAND (node, 0) = java_complete_tree (wfl_op1);
      if (TREE_OPERAND (node, 0) == error_mark_node)
        return error_mark_node;
      TREE_OPERAND (node, 1) = java_complete_tree (wfl_op2);
      if (TREE_OPERAND (node, 1) == error_mark_node)
	return error_mark_node;
      return patch_binop (node, wfl_op1, wfl_op2);

    case JAVA_UNARY_PLUS_EXPR:
    case NEGATE_EXPR:
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case CONVERT_EXPR:
      /* There are cases were wfl_op1 is a WFL. patch_unaryop knows
	 how to handle those cases. */
      wfl_op1 = TREE_OPERAND (node, 0);
      TREE_OPERAND (node, 0) = java_complete_tree (wfl_op1);
      if (TREE_OPERAND (node, 0) == error_mark_node)
	return error_mark_node;
      return patch_unaryop (node, wfl_op1);

    case ARRAY_REF:
      /* There are cases were wfl_op1 is a WFL. patch_array_ref knows
	 how to handle those cases. */
      wfl_op1 = TREE_OPERAND (node, 0);
      TREE_OPERAND (node, 0) = java_complete_tree (wfl_op1);
      if (TREE_OPERAND (node, 0) == error_mark_node)
	return error_mark_node;
      /* The same applies to wfl_op2 */
      wfl_op2 = TREE_OPERAND (node, 1);
      TREE_OPERAND (node, 1) = java_complete_tree (wfl_op2);
      if (TREE_OPERAND (node, 1) == error_mark_node)
	return error_mark_node;
      return patch_array_ref (node, wfl_op1, wfl_op2);

    case JAVA_THIS_EXPR:
      /* Can't use THIS in a static environment */
      if (!current_this)
	{
	  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);
	  parse_error_context (wfl_operator, "Keyword `this' used outside "
			       "allowed context");
	  TREE_TYPE (node) = error_mark_node;
	  return error_mark_node;
	}
      return current_this;

    case STRING_CST:
      /* Build the internal string representation */
      push_obstacks (&permanent_obstack, &permanent_obstack);
      node = get_identifier (TREE_STRING_POINTER (node));
      location = alloc_name_constant (CONSTANT_String, node);
      node = build_ref_from_constant_pool (location);
      TREE_TYPE (node) = promote_type (string_type_node);
      return node;

    default:
      fatal ("No case for tree code `%s' - java_complete_tree\n",
	     tree_code_name [TREE_CODE (node)]);
    }
  return node;
}

/* Complete function call's argument. Return a non zero value is an
   error was found.  */

static int
complete_function_arguments (node)
     tree node;
{
  int flag = 0;
  tree cn;

  for (cn = TREE_OPERAND (node, 1); cn; cn = TREE_CHAIN (cn))
    {
      tree wfl = TREE_VALUE (cn), parm;
      parm = java_complete_tree (wfl);
      if (parm == error_mark_node)
	{
	  flag = 1;
	  continue;
	}
      if (TREE_CODE (TREE_TYPE (parm)) == RECORD_TYPE)
	TREE_VALUE (cn) = convert (promote_type (TREE_TYPE (parm)), parm);
      else
	TREE_VALUE (cn) = save_expr (parm);
      if (not_initialized_as_it_should_p (parm))
	{
	  ERROR_VARIABLE_NOT_INITIALIZED (wfl, EXPR_WFL_NODE (wfl));
	  INITIALIZED_P (parm) = 1;
	}
    }
  return flag;
}

/* Sometimes (for loops and variable initialized during their
   declaration), we want to wrap a statement around a WFL and turn it
   debugable.  */

static tree
build_debugable_stmt (location, stmt)
    int location;
    tree stmt;
{
  if (TREE_CODE (stmt) != EXPR_WITH_FILE_LOCATION)
    {
      stmt = build_expr_wfl (stmt, input_filename, 0, 0);
      EXPR_WFL_LINECOL (stmt) = location;
    }
  JAVA_MAYBE_GENERATE_DEBUG_INFO (stmt);
  return stmt;
}

static tree
build_expr_block (body, decls)
     tree body, decls;
{
  tree node = make_node (BLOCK);
  BLOCK_EXPR_DECLS (node) = decls;
  BLOCK_EXPR_BODY (body);
  if (body)
    TREE_TYPE (node) = TREE_TYPE (body);
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* Create a new function block and link its supercontext to the
   previous block. The current function DECL is used as supercontext
   when enter_block is called for the first time for a given
   function. The current function body (DECL_FUNCTION_BODY) is set to
   the newly created block.  */

static block_level = 0;

static tree
enter_block ()
{
  tree b = build_expr_block (NULL_TREE, NULL_TREE);
  tree fndecl = current_function_decl; 

  if (!DECL_FUNCTION_BODY (fndecl))
    {
      BLOCK_SUPERCONTEXT (b) = fndecl;
      DECL_FUNCTION_BODY (fndecl) = b;
    }
  else
    {
      BLOCK_SUPERCONTEXT (b) = DECL_FUNCTION_BODY (fndecl);
      DECL_FUNCTION_BODY (fndecl) = b;
    }
  return b;
}

/* Exit a block by changing the current function body
   (DECL_FUNCTION_BODY) to the current block super context, only if
   the block being exited isn't the method's top level one.  */

static tree
exit_block ()
{
  tree b = DECL_FUNCTION_BODY (current_function_decl);

  if (BLOCK_SUPERCONTEXT (b) != current_function_decl)
    DECL_FUNCTION_BODY (current_function_decl) = BLOCK_SUPERCONTEXT (b);

  return b;
}

/* Lookup for NAME in the nested function's blocks, all the way up to
   the current toplevel one. It complies with Java's local variable
   scoping rules.  */

static tree
lookup_name_in_blocks (name)
     tree name;
{
  tree b = DECL_FUNCTION_BODY (current_function_decl);

  while (b != current_function_decl)
    {
      tree current;

      /* Paranoid sanity check. To be removed */
      if (TREE_CODE (b) != BLOCK)
	fatal ("non block expr function body - lookup_name_in_blocks");

      for (current = BLOCK_EXPR_DECLS (b); current; 
	   current = TREE_CHAIN (current))
	if (DECL_NAME (current) == name)
	  return current;
      b = BLOCK_SUPERCONTEXT (b);
    }
  return NULL_TREE;
}

static void
maybe_absorb_scoping_blocks ()
{
  while (BLOCK_EXPR_ORIGIN (DECL_FUNCTION_BODY (current_function_decl)))
    {
      tree b = exit_block ();
      java_method_add_stmt (current_function_decl, b);
      SOURCE_FRONTEND_DEBUG (("Absorbing scoping block at line %d", lineno));
    }
}


/* This section of the source is reserved to build_* functions that
   are building incomplete tree nodes and the patch_* functions that
   are completing them.  */

/* Build an incomplete CALL_EXPR node. Encapsulate it within a WFL */

static tree
build_method_invocation (name, args)
    tree name;
    tree args;
{
  tree call = build (CALL_EXPR, NULL_TREE, name, args, NULL_TREE);
  TREE_SIDE_EFFECTS (call) = 1;
  /* Check on cases where NAME isn't a WFL. FIXME */
  EXPR_WFL_LINECOL (call) = EXPR_WFL_LINECOL (name);
  return call;
}

/* Build an incomplete assignment expression. */

static tree
build_assignment (op, op_location, lhs, rhs)
     int op, op_location;
     tree lhs, rhs;
{
  tree assignment;
  /* Build the corresponding binop if we deal with a Compound
     Assignment operator. Mark the binop sub-tree as part of a
     Compound Assignment expression */
  if (op != ASSIGN_TK)
    {
      rhs = build_binop (BINOP_LOOKUP (op), op_location, lhs, rhs);
      COMPOUND_ASSIGN_P (rhs) = 1;
    }
  assignment = build (MODIFY_EXPR, NULL_TREE, lhs, rhs);
  TREE_SIDE_EFFECTS (assignment) = 1;
  EXPR_WFL_LINECOL (assignment) = op_location;
  return assignment;
}

/* Print an INTEGER_CST node in a static buffer, and return the buffer. */

static char *
print_int_node (node)
    tree node;
{
  static char buffer [80];
  if (TREE_CONSTANT_OVERFLOW (node))
    sprintf (buffer, "<overflow>");
    
  if (TREE_INT_CST_HIGH (node) == 0)
    sprintf (buffer, HOST_WIDE_INT_PRINT_UNSIGNED,
	     TREE_INT_CST_LOW (node));
  else if (TREE_INT_CST_HIGH (node) == -1
	   && TREE_INT_CST_LOW (node) != 0)
    {
      buffer [0] = '-';
      sprintf (&buffer [1], HOST_WIDE_INT_PRINT_UNSIGNED,
	       -TREE_INT_CST_LOW (node));
    }
  else
    sprintf (buffer, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
	     TREE_INT_CST_HIGH (node), TREE_INT_CST_LOW (node));

  return buffer;
}

/* 15.25 Assignment operators. */

static tree
patch_assignment (node, wfl_op1, wfl_op2)
     tree node;
     tree wfl_op1;
     tree wfl_op2;
{
  tree rhs = TREE_OPERAND (node, 1);
  tree lvalue = TREE_OPERAND (node, 0);
  tree lhs_type, rhs_type, new_rhs = NULL_TREE;
  int all_primitive;
  int error_found = 0;
  int lvalue_from_array = 0;

  /* Can't assign to a final. */
  if (DECL_P (lvalue) && FIELD_FINAL (lvalue))
    {
      parse_error_context 
        (wfl_op1, "Can't assign a value to the final variable `%s'",
	 IDENTIFIER_POINTER (EXPR_WFL_NODE (wfl_op1)));
      error_found = 1;
    }

  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  /* Lhs can be a named variable */
  if (DECL_P (lvalue))
    {
      INITIALIZED_P (lvalue) = 1;
      lhs_type = TREE_TYPE (lvalue);
    }
  /* Or Lhs can be a array acccess. Should that be lvalue ? FIXME +
     comment on reason why */
  else if (TREE_CODE (wfl_op1) == ARRAY_REF)
    {
      lhs_type = TREE_TYPE (lvalue);
      lvalue_from_array = 1;
    }
  /* Or a field access */
  else if (TREE_CODE (lvalue) == COMPONENT_REF)
    lhs_type = TREE_TYPE (lvalue);
  /* Or a function return slot */
  else if (TREE_CODE (lvalue) == RESULT_DECL)
    lhs_type = TREE_TYPE (lvalue);
  /* Otherwise, this is an error */
  else
    {
      parse_error_context (wfl_op1, "Invalid left hand side of assignment");
      error_found = 1;
    }

  rhs_type = TREE_TYPE (rhs);

  /* 5.2 Begin Assignment conversion */

  /* 5.1.1 Try Identity Conversion */
  if (lhs_type == rhs_type) 
    new_rhs = rhs;
  
  /* 5.1.2 Try Widening Primitive Conversion */
  all_primitive = JPRIMITIVE_TYPE_P (lhs_type) && JPRIMITIVE_TYPE_P (rhs_type);
  if (all_primitive && JINTEGRAL_TYPE_P (rhs_type)
      && ((TYPE_PRECISION (rhs_type) < TYPE_PRECISION (lhs_type))
	  || (JFLOAT_TYPE_P (lhs_type) &&
	      TYPE_PRECISION (rhs_type) == TYPE_PRECISION (lhs_type))))
    new_rhs = convert (lhs_type, rhs);
  else if (all_primitive && JFLOAT_TYPE_P (rhs_type)
	   && (TYPE_PRECISION (rhs_type) < TYPE_PRECISION (lhs_type)))
    new_rhs = convert (lhs_type, rhs);

  /* Try a narrowing primitive conversion: 
       - expression is a constant expression of type int AND
       - variable is byte, short or char AND
       - The value of the expression is representable in the type of the 
         variable */
  else if (rhs_type == int_type_node && TREE_CONSTANT (rhs)
	   && (lhs_type == byte_type_node || lhs_type == char_type_node
	       || lhs_type == short_type_node))
    {
      if (int_fits_type_p (rhs, lhs_type))
        new_rhs = convert (lhs_type, rhs);
      else
	parse_warning_context 
	  (wfl_op1, "Constant expression `%s' to wide for narrowing "
	   "primitive conversion to `%s'", 
	   print_int_node (rhs), lang_printable_name (lhs_type));
      /* Reported a warning that will turn into an error further
	 down, so we don't return */
    }

  /* 5.2 Try a reference conversion */
  else if (!JPRIMITIVE_TYPE_P (rhs_type) && JREFERENCE_TYPE_P (lhs_type))
    {
      /* `null' may be assigned to any reference type */
      if (rhs == null_pointer_node)
        new_rhs = null_pointer_node;
      /* Try the reference assignment conversion */
      else if (valid_ref_assignconv_cast_p (rhs_type, lhs_type, 0))
	new_rhs = rhs;
      if (new_rhs)
	lhs_type = promote_type (rhs_type);
    }

  /* 15.25.2 If we have a compound assignment, convert RHS into the
     type of the LHS */
  else if (COMPOUND_ASSIGN_P (TREE_OPERAND (node, 1)))
    new_rhs = convert (lhs_type, rhs);

  /* Explicit cast required. This is an error */
  if (!new_rhs)
    {
      char *t1 = strdup ((char *)lang_printable_name (TREE_TYPE (rhs)));
      char *t2 = strdup ((char *)lang_printable_name (lhs_type));
      tree wfl;
      char operation [32];	/* Max size known */

      /* If the assignment is part of a declaration, we use the WFL of
	 the declared variable to point out the error and call it a
	 declaration problem. If the assignment is a genuine =
	 operator, we call is a operator `=' problem, otherwise we
	 call it an assignment problem. In both of these last cases,
	 we use the WFL of the operator to indicate the error. */

      if (MODIFY_EXPR_FROM_INITIALIZATION_P (node))
	{
	  wfl = wfl_op1;
	  strcpy (operation, "declaration");
	}
      else
	{
	  wfl = wfl_operator;
	  if (COMPOUND_ASSIGN_P (TREE_OPERAND (node, 1)))
	    strcpy (operation, "assignment");
	  else if (TREE_CODE (TREE_OPERAND (node, 0)) == RESULT_DECL)
	    strcpy (operation, "`return'");
	  else
	    strcpy (operation, "`='");
	}

      parse_error_context 
	(wfl, (!can_cast_to_p (rhs_type, lhs_type) ?
	       "Incompatible type for %s. Can't convert `%s' to `%s'" :
	       "Incompatible type for %s. Explicit cast "
	       "needed to convert `%s' to `%s'"), operation, t1, t2);
      free (t1); free (t2);
      error_found = 1;
    }

  /* Before reporting type incompatibility errors, check that the rhs
     is initialized, if a variable */
  if (not_initialized_as_it_should_p (rhs))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (wfl_op2, DECL_NAME (rhs));
      INITIALIZED_P (rhs) = 1;
    }

  if (error_found)
    return error_mark_node;

  /* If we built a compound expression as the result of a reference
     assignment into an array element, return it here. */
  if (TREE_CODE (node) == COMPOUND_EXPR)
    return node;
      
  TREE_OPERAND (node, 0) = lvalue;
  TREE_OPERAND (node, 1) = new_rhs;
  TREE_TYPE (node) = lhs_type;
  return node;
}

/* Check that SOURCE can be converted into DEST, at least with a
   cast. If the convertion can't occur at all, return 0 otherwise
   1. This function is used to produce accurate error messages on the
   reasons why an assignment failed. */

static int
can_cast_to_p (source, dest)
     tree source;
     tree dest;
{
  if (TREE_CODE (source) == POINTER_TYPE)
    source = TREE_TYPE (source);
  if (TREE_CODE (dest) == POINTER_TYPE)
    dest = TREE_TYPE (dest);

  if (TREE_CODE (source) == RECORD_TYPE && TREE_CODE (dest) == RECORD_TYPE)
    return valid_ref_assignconv_cast_p (source, dest, 1);

  else if (JNUMERIC_TYPE_P (source) && JNUMERIC_TYPE_P (dest))
    return 1;

  return 0;
}

/* Check that something of SOURCE type can be assigned or cast to
   something of DEST type at runtime. Return 1 if the operation is
   valid, 0 otherwise. If CAST is set to 1, we're treating the case
   were SOURCE is cast into DEST, which borrows a lot of the
   assignment check. */

static int
valid_ref_assignconv_cast_p (source, dest, cast)
     tree source;
     tree dest;
     int cast;
{
  if (TREE_CODE (source) == POINTER_TYPE)
    source = TREE_TYPE (source);
  if (TREE_CODE (dest) == POINTER_TYPE)
    dest = TREE_TYPE (dest);
  /* Case where SOURCE is a class type */
  if (TYPE_CLASS_P (source))
    {
      if (TYPE_CLASS_P (dest))
	return  source == dest || inherits_from_p (source, dest)
	  || cast && inherits_from_p (dest, source);
      if (TYPE_INTERFACE_P (dest))
	{
	  /* If doing a cast and SOURCE is final, the operation is
             always correct a compile time (because even if SOURCE
             does not implement DEST, a subclass of SOURCE might). */
	  if (cast && !CLASS_FINAL (TYPE_NAME (source)))
	    return 1;
	  /* Otherwise, SOURCE must implement DEST */
	  return interface_of_p (dest, source);
	}
      /* DEST is an array, cast permited if SOURCE is of Object type */
      return (cast && source == object_type_node ? 1 : 0);
    }
  if (TYPE_INTERFACE_P (source))
    {
      if (TYPE_CLASS_P (dest))
	{
	  /* If not casting, DEST must be the Object type */
	  if (!cast)
	    return dest == object_type_node;
	  /* We're doing a cast. The cast is always valid is class
	     DEST is not final, otherwise, DEST must implement SOURCE */
	  else if (!CLASS_FINAL (TYPE_NAME (source)))
	    return 1;
	  else
	    return interface_of_p (source, dest);
	}
      if (TYPE_INTERFACE_P (dest))
	{
	  /* If doing a cast, then if SOURCE and DEST contain method
             with the same signature but different return type, then
             this is a (compile time) error */
	  if (cast)
	    {
	      tree method_source, method_dest;
	      tree source_type;
	      tree source_sig, dest_sig;
	      tree source_name;
	      for (method_source = TYPE_METHODS (source); method_source; 
		   method_source = TREE_CHAIN (method_source))
		{
		  source_sig = 
		    build_java_argument_signature (TREE_TYPE (method_source));
		  source_type = TREE_TYPE (TREE_TYPE (method_source));
		  source_name = DECL_NAME (method_source);
		  for (method_dest = TYPE_METHODS (dest);
		       method_dest; method_dest = TREE_CHAIN (method_dest))
		    if (source_sig == 
			build_java_argument_signature (TREE_TYPE (method_dest))
			&& source_name == DECL_NAME (method_dest)
			&& source_type != TREE_TYPE (TREE_TYPE (method_dest)))
		      return 0;
		}
	      return 1;
	    }
	  else
	    return source == dest || interface_of_p (dest, source);
	}
      else			/* Array */
	return 0;
    }
  if (TYPE_ARRAY_P (source))
    {
      if (TYPE_CLASS_P (dest))
	return dest == object_type_node;
      if (TYPE_INTERFACE_P (dest))
	return 0;		/* Install test on Clonable. FIXME */
      else			/* Arrays */
	{
	  tree source_element_type = TYPE_ARRAY_ELEMENT (source);
	  tree dest_element_type = TYPE_ARRAY_ELEMENT (dest);
	  
	  if (source_element_type == dest_element_type)
	    return 1;
	  return valid_ref_assignconv_cast_p (source_element_type,
					      dest_element_type, cast);
	}
      return 0;
    }
  return 0;
}

/* Build an incomplete binop expression. */

static tree
build_binop (op, op_location, op1, op2)
     enum tree_code op;
     int op_location;
     tree op1, op2;
{
  tree wfl;

  /* URSHIFT_EXPR is not part of what GCC understands, we can't directly build
     a node with it */
  tree binop = 
    build ((op == URSHIFT_EXPR ? RSHIFT_EXPR : op), NULL_TREE, op1, op2);
  if (op == URSHIFT_EXPR)
    TREE_SET_CODE (binop, op);

  TREE_SIDE_EFFECTS (binop) = 1;
  /* Store the location of the operator, for better error report. The
     string of the operator will be rebuild based on the OP value. */
  EXPR_WFL_LINECOL (binop) = op_location;
  return binop;
}

/* Build the string of the operator retained by NODE. If NODE is part
   of a compound expression, add an '=' at the end of the string. This
   function is called when an error needs to be reported on an
   operator. The string is returned as a pointer to a static character
   buffer. */

static char *
operator_string (node)
     tree node;
{
#define BUILD_OPERATOR_STRING(S)					\
  {									\
    sprintf (buffer, "%s%s", S, (COMPOUND_ASSIGN_P (node) ? "=" : ""));	\
    return buffer;							\
  }
  
  static char buffer [10];
  switch (TREE_CODE (node))
    {
    case MULT_EXPR: BUILD_OPERATOR_STRING ("*");
    case RDIV_EXPR: BUILD_OPERATOR_STRING ("/");
    case TRUNC_MOD_EXPR: BUILD_OPERATOR_STRING ("%");
    case PLUS_EXPR: BUILD_OPERATOR_STRING ("+");
    case MINUS_EXPR: BUILD_OPERATOR_STRING ("-");
    case LSHIFT_EXPR: BUILD_OPERATOR_STRING ("<<");
    case RSHIFT_EXPR: BUILD_OPERATOR_STRING (">>");
    case URSHIFT_EXPR: BUILD_OPERATOR_STRING (">>>");
    case BIT_AND_EXPR: BUILD_OPERATOR_STRING ("&");
    case BIT_XOR_EXPR: BUILD_OPERATOR_STRING ("^");
    case BIT_IOR_EXPR: BUILD_OPERATOR_STRING ("|");
    case TRUTH_ANDIF_EXPR: BUILD_OPERATOR_STRING ("&&");
    case TRUTH_ORIF_EXPR: BUILD_OPERATOR_STRING ("||");
    case EQ_EXPR: BUILD_OPERATOR_STRING ("==");
    case NE_EXPR: BUILD_OPERATOR_STRING ("!=");
    case GT_EXPR: BUILD_OPERATOR_STRING (">");
    case GE_EXPR: BUILD_OPERATOR_STRING (">=");
    case LT_EXPR: BUILD_OPERATOR_STRING ("<");
    case LE_EXPR: BUILD_OPERATOR_STRING ("<=");
    case JAVA_UNARY_PLUS_EXPR: BUILD_OPERATOR_STRING ("+");
    case NEGATE_EXPR: BUILD_OPERATOR_STRING ("-");
    case TRUTH_NOT_EXPR: BUILD_OPERATOR_STRING ("!");
    case BIT_NOT_EXPR: BUILD_OPERATOR_STRING ("~");
    case PREINCREMENT_EXPR:	/* Fall through */
    case POSTINCREMENT_EXPR: BUILD_OPERATOR_STRING ("++");
    case PREDECREMENT_EXPR:	/* Fall through */
    case POSTDECREMENT_EXPR: BUILD_OPERATOR_STRING ("--");
    default:
      fatal ("unregistered operator %s - operator_string",
	     tree_code_name [TREE_CODE (node)]);
    }
  return NULL;
#undef BUILD_OPERATOR_STRING
}

/* Binary operators (15.16 up to 15.18). We return error_mark_node on
   errors but we modify NODE so that it contains the type computed
   according to the expression, when it's fixed. Otherwise, we write
   error_mark_node as the type. It allows us to further the analysis
   of remaining nodes and detects more errors in certain cases.  */

static tree
patch_binop (node, wfl_op1, wfl_op2)
     tree node;
     tree wfl_op1;
     tree wfl_op2;
{
  tree op1 = TREE_OPERAND (node, 0);
  tree op2 = TREE_OPERAND (node, 1);
  tree op1_type = TREE_TYPE (op1);
  tree op2_type = TREE_TYPE (op2);
  tree prom_type;
  int code = TREE_CODE (node);
  /* If 1, tell the routine that we have to return error_mark_node
     after checking for the initialization of the RHS */
  int error_found = 0;

  /* Figure what is going to be checked first for initialization prior
     its use. If NODE is part of a compound assignment, we check the
     second operand first, otherwise the first one first. We also
     initialize the matching WFL for the error report. `cfi' stands
     for Check For Initialization */
  tree cfi = (COMPOUND_ASSIGN_P (node) ? op2 : op1);
  tree cfi_wfl = (COMPOUND_ASSIGN_P (node) ? wfl_op2 : wfl_op1);

  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  /* Check initialization of LHS first. We then silence further error
     message if the variable wasn't initialized */
  if (not_initialized_as_it_should_p (cfi))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (cfi_wfl, DECL_NAME (cfi));
      INITIALIZED_P (op1) = 1;
    }

  switch (code)
    {
    /* 15.16 Multiplicative operators */
    case MULT_EXPR:		/* 15.16.1 Multiplication Operator * */
    case RDIV_EXPR:		/* 15.16.2 Division Operator / */
    case TRUNC_MOD_EXPR:	/* 15.16.3 Remainder operator % */
      if (!JPRIMITIVE_TYPE_P (op1_type) || !JPRIMITIVE_TYPE_P (op2_type))
	{
	  if (!JPRIMITIVE_TYPE_P (op1_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op1_type);
	  if (!JPRIMITIVE_TYPE_P (op2_type) && (op1_type != op2_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op2_type);
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	  break;
	}
      prom_type = binary_numeric_promotion (op1_type, op2_type, &op1, &op2);
      /* Change the division operator if necessary */
      if (code == RDIV_EXPR && TREE_CODE (prom_type) == INTEGER_TYPE)
	TREE_SET_CODE (node, TRUNC_DIV_EXPR);
      /* This one is more complicated. FLOATs are processed by a function
	 call to soft_fmod. */
      if (code == TRUNC_MOD_EXPR)
	return build_java_binop (TRUNC_MOD_EXPR, prom_type, op1, op2);
      break;

    /* 15.17 Additive Operators */
    case PLUS_EXPR:		/* 15.17.1 String Concatenation Operator + */
      if (JSTRING_TYPE_P (op1_type) || JSTRING_TYPE_P (op2_type))
	fatal ("operator `+' non implemented on String - patch_binop");
    case MINUS_EXPR:		/* 15.17.2 Additive Operators (+ and -) for
				   Numeric Types */
      if (!JPRIMITIVE_TYPE_P (op1_type) || !JPRIMITIVE_TYPE_P (op2_type))
	{
	  if (!JPRIMITIVE_TYPE_P (op1_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op1_type);
	  if (!JPRIMITIVE_TYPE_P (op2_type) && (op1_type != op2_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op2_type);
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	  break;
	}
      prom_type = binary_numeric_promotion (op1_type, op2_type, &op1, &op2);
      break;

    /* 15.18 Shift Operators */
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case URSHIFT_EXPR:
      if (!JINTEGRAL_TYPE_P (op1_type) || !JINTEGRAL_TYPE_P (op2_type))
	{
	  if (!JINTEGRAL_TYPE_P (op1_type))
	    ERROR_CAST_NEEDED_TO_INTEGRAL (wfl_operator, node, op1_type);
	  else
	    parse_error_context 
	      (wfl_operator, (JPRIMITIVE_TYPE_P (op2_type) ? 
	       "Incompatible type for `%s'. Explicit cast needed to convert "
	       "shift distance from `%s' to integral" : 
	       "Incompatible type for `%s'. Can't convert shift distance from "
	       "`%s' to integral"), 
	       operator_string (node), lang_printable_name (op2_type));
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	  break;
	}

      /* Unary numeric promotion (5.6.1) is performed on each operand
         separatly */
      op1 = convert (promote_type (op1_type), op1);
      op2 = convert (promote_type (op2_type), op2);

      /* The type of the shift expression is the type of the promoted
         type of the left-hand operand */
      prom_type = TREE_TYPE (op1);

      /* Shift int only up to 0x1f and long up to 0x3f */
      if (prom_type == int_type_node)
	op2 = fold (build (BIT_AND_EXPR, int_type_node, op2, 
			   build_int_2 (0x1f, 0)));
      else
	op2 = fold (build (BIT_AND_EXPR, int_type_node, op2, 
			   build_int_2 (0x3f, 0)));

      /* The >>> operator is a >> operating on unsigned quantities */
      if (code == URSHIFT_EXPR)
	{
	  op1 = convert (unsigned_type (prom_type), op1);
	  TREE_SET_CODE (node, RSHIFT_EXPR);
	}
      break;
      

      /* 15.21 Bitwise and Logical Operators */
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case BIT_IOR_EXPR:
      if (JINTEGRAL_TYPE_P (op1_type) && JINTEGRAL_TYPE_P (op2_type))
	/* Binary numeric promotion is performed on both operand and the
	   expression retain that type */
	prom_type = binary_numeric_promotion (op1_type, op2_type, &op1, &op2);

      else if (TREE_CODE (op1_type) == BOOLEAN_TYPE 
	       && TREE_CODE (op1_type) == BOOLEAN_TYPE)
	/* The type of the bitwise operator expression is BOOLEAN */
	prom_type = boolean_type_node;
      else
	{
	  if (!JINTEGRAL_TYPE_P (op1_type))
	    ERROR_CAST_NEEDED_TO_INTEGRAL (wfl_operator, node, op1_type);
	  if (!JINTEGRAL_TYPE_P (op2_type) && (op1_type != op2_type))
	    ERROR_CAST_NEEDED_TO_INTEGRAL (wfl_operator, node, op2_type);
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	  /* Insert a break here if adding thing before the switch's
             break for this case */
	}
      break;

      /* 15.22 Conditional-And Operator */
    case TRUTH_ANDIF_EXPR:
      /* 15.23 Conditional-Or Operator */
    case TRUTH_ORIF_EXPR:
      /* Operands must be of BOOLEAN type */
      if (TREE_CODE (op1_type) != BOOLEAN_TYPE || 
	  TREE_CODE (op2_type) != BOOLEAN_TYPE)
	{
	  if (TREE_CODE (op1_type) != BOOLEAN_TYPE)
	    ERROR_CANT_CONVERT_TO_BOOLEAN (wfl_operator, node, op1_type);
	  if (TREE_CODE (op2_type) != BOOLEAN_TYPE && (op1_type != op2_type))
	    ERROR_CANT_CONVERT_TO_BOOLEAN (wfl_operator, node, op2_type);
	  TREE_TYPE (node) = boolean_type_node;
	  error_found = 1;
	  break;
	}
      /* The type of the conditional operators is BOOLEAN */
      prom_type = boolean_type_node;
      break;

      /* 15.19.1 Numerical Comparison Operators <, <=, >, >= */
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      /* The type of each of the operands must be a primitive numeric
         type */
      if (!JNUMERIC_TYPE_P (op1_type) || ! JNUMERIC_TYPE_P (op2_type))
	{
	  if (!JNUMERIC_TYPE_P (op1_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op1_type);
	  if (!JNUMERIC_TYPE_P (op2_type) && (op1_type != op2_type))
	    ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op2_type);
	  TREE_TYPE (node) = boolean_type_node;
	  error_found = 1;
	  break;
	}
      /* Binary numeric promotion is performed on the operands */
      binary_numeric_promotion (op1_type, op2_type, &op1, &op2);
      /* The type of the relation expression is always BOOLEAN */
      prom_type = boolean_type_node;
      break;

      /* 15.20 Equality Operator */
    case EQ_EXPR:
    case NE_EXPR:
      /* 15.20.1 Numerical Equality Operators == and != */
      /* Binary numeric promotion is performed on the operands */
      if (JPRIMITIVE_TYPE_P (op1_type) && JPRIMITIVE_TYPE_P (op2_type))
	binary_numeric_promotion (op1_type, op2_type, &op1, &op2);
      
      /* 15.20.2 Boolean Equality Operators == and != */
      else if (TREE_CODE (op1_type) == BOOLEAN_TYPE &&
	  TREE_CODE (op2_type) == BOOLEAN_TYPE)
	;			/* Nothing to do here */
      
      /* 15.20.3 Reference Equality Operators == and != */
      /* Types have to be either references or the null type */
      else if ((op1 == null_pointer_node || op2 == null_pointer_node 
		|| JREFERENCE_TYPE_P (op1_type) 
		|| JREFERENCE_TYPE_P (op2_type))
	       && ((op1_type == op2_type)
		   /* The should use a can_cast_to_p() */
		   ))
	;			/* Nothing to do here */
	  
      /* Else we have an error figure what can't be converted into
	 what and report the error */
      else
	{
	  char *t1;
	  t1 = strdup ((char *)lang_printable_name (op1_type));
	  parse_error_context 
	    (wfl_operator, "Incompatible type for `%s'. Can't convert `%s' "
	     "to `%s'", operator_string (node), t1, 
	     lang_printable_name (op2_type));
	  free (t1);
	  TREE_TYPE (node) = boolean_type_node;
	  error_found = 1;
	  break;
	}
      prom_type = boolean_type_node;
      break;
    }

  /* Then check the initialization of the RHS. We don't do that if
     we're dealing with a node that is part of a compound
     assignment. We then silence further error message if the variable
     wasn't initialized */
  if (not_initialized_as_it_should_p (op2) && !COMPOUND_ASSIGN_P (node))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (wfl_op2, DECL_NAME (op2));
      INITIALIZED_P (op2) = 1;
    }

  if (error_found)
    return error_mark_node;

  TREE_OPERAND (node, 0) = op1;
  TREE_OPERAND (node, 1) = op2;
  TREE_TYPE (node) = prom_type;
  return fold (node);
}

/* Build an incomplete unary operator expression. Unary `+' node is
   build as a CONV_EXPR, even though its tree code is overridden by a
   JAVA_UNARY_PLUS_EXPR that isn't a tree code, to differentiate it during
   the walk. */

static tree
build_unaryop (op_token, op_location, op1)
     int op_token, op_location;
     tree op1;
{
  enum tree_code op;
  tree unaryop;
  switch (op_token)
    {
    case PLUS_TK: op = CONVERT_EXPR; break;
    case MINUS_TK: op = NEGATE_EXPR; break;
    case NEG_TK: op = TRUTH_NOT_EXPR; break;
    case NOT_TK: op = BIT_NOT_EXPR; break;
    default: fatal ("Unknown token `%d' for unary operator - build_unaryop",
		    op_token);
    }

  unaryop = build1 (op, NULL_TREE, op1);
  if (op_token == PLUS_TK)
    TREE_SET_CODE (unaryop, JAVA_UNARY_PLUS_EXPR);

  TREE_SIDE_EFFECTS (unaryop) = 1;
  /* Store the location of the operator, for better error report. The
     string of the operator will be rebuild based on the OP value. */
  EXPR_WFL_LINECOL (unaryop) = op_location;
  return unaryop;
}

/* Special case for the ++/-- operators, since they require an extra
   argument to build, which is set to NULL and patched
   later. IS_POST_P is 1 if the operator, 0 otherwise.  */

static tree
build_incdec (op_token, op_location, op1, is_post_p)
     int op_token, op_location;
     tree op1;
     int is_post_p;
{
  static enum tree_code lookup [2][2] = 
    {
      { PREDECREMENT_EXPR, PREINCREMENT_EXPR, },
      { POSTDECREMENT_EXPR, POSTINCREMENT_EXPR, },
    };
  tree node = build (lookup [is_post_p][(op_token - DECR_TK)], 
		     NULL_TREE, op1, NULL_TREE);
  TREE_SIDE_EFFECTS (node) = 1;
  /* Store the location of the operator, for better error report. The
     string of the operator will be rebuild based on the OP value. */
  EXPR_WFL_LINECOL (node) = op_location;
  return node;
}     

/* Build an incomplete cast operator, based on the use of the
   CONVERT_EXPR. Note that TREE_TYPE of the constructed node is
   set. java_complete_tree is trained to walk a CONVERT_EXPR even
   though its type is already set.  */

static tree
build_cast (location, type, exp)
     int location;
     tree type, exp;
{
  tree node = build1 (CONVERT_EXPR, type, exp);
  EXPR_WFL_LINECOL (node) = location;
  return node;
}

/* 15.14 Unary operators. We return error_mark_node in case of error,
   but preserve the type of NODE if the type is fixed.  */

static tree
patch_unaryop (node, wfl_op)
     tree node;
     tree wfl_op;
{
  tree op = TREE_OPERAND (node, 0);
  tree op_type = TREE_TYPE (op);
  tree prom_type, value;
  int code = TREE_CODE (node);
  int error_found = 0;

  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  switch (code)
    {
      /* 15.13.2 Postfix Increment Operator ++ */
    case POSTINCREMENT_EXPR:
      /* 15.13.3 Postfix Increment Operator -- */
    case POSTDECREMENT_EXPR:
      /* 15.14.1 Prefix Increment Operator ++ */
    case PREINCREMENT_EXPR:
      /* 15.14.2 Prefix Decrement Operator -- */
    case PREDECREMENT_EXPR:
      if (!DECL_P (op))
	{
	  parse_error_context (wfl_operator, "Invalid argument to `%s'",
			       operator_string (node));
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      else if (FIELD_FINAL (op))
	{
	  parse_error_context 
	    (wfl_op, "Can't assign a value to the final variable `%s'",
	     IDENTIFIER_POINTER (EXPR_WFL_NODE (wfl_op)));
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      /* From now on, we know that op if a variable and that it has a
         valid wfl. We use wfl_op to locate errors related to the
         ++/-- operand. */
      else if (!JNUMERIC_TYPE_P (op_type))
	{
	  parse_error_context
	    (wfl_op, "Invalid argument type `%s' to `%s'",
	     lang_printable_name (op_type), operator_string (node));
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      else
	{
	  /* Before the addition, binary numeric promotion if performed on
	     both operands */
	  value = integer_one_node;
	  prom_type = binary_numeric_promotion (op_type, TREE_TYPE (value), 
						&op, &value);
	  /* And write the promoted increment back */
	  TREE_OPERAND (node, 1) = value;
	}
      break;

      /* 15.14.3 Unary Plus Operator + */
    case JAVA_UNARY_PLUS_EXPR:
      /* 15.14.4 Unary Minus Operator - */
    case NEGATE_EXPR:
      if (!JNUMERIC_TYPE_P (op_type))
	{
	  ERROR_CANT_CONVERT_TO_NUMERIC (wfl_operator, node, op_type);
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      /* Unary numeric promotion is performed on operand */
      else
	{
	  prom_type = promote_type (op_type);
	  op = convert (prom_type, op);
	  if (code == JAVA_UNARY_PLUS_EXPR)
	    node = op;
	}
      break;

      /* 15.14.5 Bitwise Complement Operator ~ */
    case BIT_NOT_EXPR:
      if (!JINTEGRAL_TYPE_P (op_type))
	{
	  ERROR_CAST_NEEDED_TO_INTEGRAL (wfl_operator, node, op_type);
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      else
	{
	  prom_type = promote_type (op_type);
	  op = convert (prom_type, op);
	}
      break;

      /* 15.14.6 Logical Complement Operator ! */
    case TRUTH_NOT_EXPR:
      if (TREE_CODE (op_type) != BOOLEAN_TYPE)
	{
	  ERROR_CANT_CONVERT_TO_BOOLEAN (wfl_operator, node, op_type);
	  TREE_TYPE (node) = boolean_type_node;
	  error_found = 1;
	}
      else
	prom_type = boolean_type_node;
      break;

      /* 15.15 Cast Expression */
    case CONVERT_EXPR:
      value = patch_cast (node, wfl_op, wfl_operator);
      if (value == error_mark_node)
	{
	  TREE_TYPE (node) = error_mark_node;
	  error_found = 1;
	}
      else
	node = value;
      break;
    }
  
  /* Check variable initialization */
  if (not_initialized_as_it_should_p (op))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (wfl_op, DECL_NAME (op));
      INITIALIZED_P (op) = 1;
    }
  
  if (error_found)
    return error_mark_node;
  /* In the case of JAVA_UNARY_PLUS_EXPR, we replaced NODE by a new one */
  else if (code != JAVA_UNARY_PLUS_EXPR && code != CONVERT_EXPR)
    {
      TREE_OPERAND (node, 0) = op;
      TREE_TYPE (node) = prom_type;
    }
  return fold (node);
}

/* Generic type resolution that sometimes takes place during node
   patching. Returned the resolved type or generate an error
   message. Return the resolved type or NULL_TREE.  */

static tree
resolve_type_during_patch (type)
     tree type;
{
  if (unresolved_type_p (type, NULL))
    {
      tree type_decl = resolve_no_layout (EXPR_WFL_NODE (type), NULL_TREE);
      if (!type_decl)
	{
	  parse_error_context (type, 
			       "Class `%s' not found in type declaration",
			       IDENTIFIER_POINTER (EXPR_WFL_NODE (type)));
	  return NULL_TREE;
	}
      else
	return TREE_TYPE (type_decl);
    }
  return type;
}
/* 5.5 Casting Conversion. error_mark_node is returned if an error is
   found. Otherwise NODE or something meant to replace it is returned.  */

static tree
patch_cast (node, wfl_op, wfl_operator)
     tree node;
     tree wfl_op;
     tree wfl_operator;
{
  tree op = TREE_OPERAND (node, 0);
  tree op_type = TREE_TYPE (op);
  tree cast_type = TREE_TYPE (node);
  char *t1;

  /* First resolve OP_TYPE if unresolved */
  if (!(cast_type = resolve_type_during_patch (cast_type)))
    return error_mark_node;

  /* Check on cast that are proven correct at compile time */
  if (JNUMERIC_TYPE_P (cast_type) && JNUMERIC_TYPE_P (op_type))
    {
      static tree convert_narrow ();
      /* Same type */
      if (cast_type == op_type)
	return node;

      /* Try widening/narowwing convertion. Potentially, things need
	 to be worked out in gcc so we implement the extreme cases
	 correctly. fold_convert() needs to be fixed. */
      return convert (cast_type, op);
    }

  /* The remaining legal casts involve conversion between reference
     types. Check for their compile time correctness. */
  if (JREFERENCE_TYPE_P (op_type) && JREFERENCE_TYPE_P (cast_type) 
      && valid_ref_assignconv_cast_p (op_type, cast_type, 1))
    {
      TREE_TYPE (node) = promote_type (cast_type);
      /* Now, the case can be determined correct at compile time if
         OP_TYPE can be converted into CAST_TYPE by assignment
         conversion (5.2) */

      if (valid_ref_assignconv_cast_p (op_type, cast_type, 0))
	return node;

      /* The cast requires a run-time check */
      return build (CALL_EXPR, promote_type (cast_type),
		    build_address_of (soft_checkcast_node),
		    tree_cons (NULL_TREE, build_class_ref (cast_type),
			       build_tree_list (NULL_TREE, op)),
		    NULL_TREE);
    }

  /* Any other casts are proven incorrect at compile time */
  t1 = strdup ((char *)lang_printable_name (op_type));
  parse_error_context (wfl_operator, "Invalid cast from `%s' to `%s'",
		       t1, lang_printable_name (cast_type));
  free (t1);
  return error_mark_node;
}

/* Build an ARRAY_REF incomplete tree node. Note that operand 1 isn't
   a list of indices. */
static tree
build_array_ref (location, array, index)
     int location;
     tree array, index;
{
  tree node = build (ARRAY_REF, NULL_TREE, array, index);
  EXPR_WFL_LINECOL (node) = location;
  return node;
}

/* 15.12 Array Access Expression */

static tree
patch_array_ref (node, wfl_array, wfl_index)
     tree node, wfl_array, wfl_index;
{
  tree array = TREE_OPERAND (node, 0);
  tree array_type  = TREE_TYPE (array);
  tree index = TREE_OPERAND (node, 1);
  tree index_type = TREE_TYPE (index);
  tree promoted_index_type;
  int error_found = 0;

  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  if (not_initialized_as_it_should_p (array))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (wfl_array, DECL_NAME (array));
      INITIALIZED_P (array) = 1;
    }
  if (! flag_emit_class_files)
    array = save_expr (array);

  if (TREE_CODE (array_type) == POINTER_TYPE)
    array_type = TREE_TYPE (array_type);

  /* The array reference must be an array */
  if (!TYPE_ARRAY_P (array_type))
    {
      parse_error_context 
	(wfl_operator, "`[]' can only be applied to arrays. It can't be "
	 "applied to `%s'", lang_printable_name (array_type));
      TREE_TYPE (node) = error_mark_node;
      error_found = 1;
    }

  /* The array index underdoes unary numeric promotion. The promoted
     type must be int */
  promoted_index_type = promote_type (index_type);
  if (promoted_index_type != int_type_node)
    {
      int could_cast = can_cast_to_p (index_type, int_type_node);
      parse_error_context 
	(wfl_operator, 
	 (could_cast ? "Incompatible type for `[]'. Explicit cast needed to "
	  "convert `%s' to `int'" : "Incompatible type for `[]'. "
	  "Can't convert `%s' to `int'"),
	 lang_printable_name (index_type));
      TREE_TYPE (node) = error_mark_node;
      error_found = 1;
    }

  /* Now if the index is a var/parm decl, check on its initialization */
  if (not_initialized_as_it_should_p (index))
    {
      ERROR_VARIABLE_NOT_INITIALIZED (wfl_index, DECL_NAME (index));
      INITIALIZED_P (index) = 1;
    }

  if (error_found)
    return error_mark_node;
  index = convert (promoted_index_type, index);

  if (TREE_CODE (array_type) == RECORD_TYPE)
    array_type = promote_type (TYPE_ARRAY_ELEMENT (array_type));
  if (flag_emit_class_files)
    {
      TREE_OPERAND (node, 0)= array;
      TREE_OPERAND (node, 1)= index;
    }
  else
    node = build_java_arrayaccess (array, array_type, index);
  TREE_TYPE (node) = array_type;
  return node;
}

/* 15.9 Array Creation Expressions */

static tree
build_newarray_node (type, dims, extra_dims)
     tree type;
     tree dims;
     int extra_dims;
{
  tree node =
    build (CALL_EXPR, NULL_TREE, type, nreverse (dims), 
	   build_int_2 (extra_dims, 0));
  TREE_SET_CODE (node, JAVA_NEW_ARRAY_EXPR);
  return node;
}

static tree
patch_newarray (node)
     tree node;
{
  tree type = TREE_OPERAND (node, 0);
  tree dims = TREE_OPERAND (node, 1);
  tree cdim, array_type;
  int error_found = 0;
  int ndims = 0;
  int xdims = TREE_INT_CST_LOW (TREE_OPERAND (node, 2));
  int total_dims;

  /* Dimension types are verified. It's better for the types to be
     verified in order. */
  for (cdim = dims, ndims = 0; cdim; cdim = TREE_CHAIN (cdim), ndims++ )
    {
      int dim_error = 0;
      tree dim = TREE_VALUE (cdim);

      /* Dim might have been saved during its evaluation */
      dim = (TREE_CODE (dim) == SAVE_EXPR ? dim = TREE_OPERAND (dim, 0) : dim);

      /* The type of each specified dimension must be an integral type. */
      if (!JINTEGRAL_TYPE_P (TREE_TYPE (dim)))
	dim_error = 1;

      /* Each expression undergoes an unary numeric promotion (5.6.1) and the
	 promoted type must be int. */
      else
	{
	  dim = convert (promote_type (TREE_TYPE (dim)), dim);
	  if (TREE_TYPE (dim) != int_type_node)
	    dim_error = 1;
	}

      /* Report errors on types here */
      if (dim_error)
	{
	  parse_error_context 
	    (TREE_PURPOSE (cdim), 
	     "Incompatible type for dimension in array creation expression. "
	     "%s convert `%s' to `int'", 
	     (can_cast_to_p (TREE_TYPE (dim), int_type_node) ?
	      "Explicit cast needed to" : "Can't"),
	     lang_printable_name (TREE_TYPE (dim)));
	  error_found = 1;
	}

      /* Check for uninitialized variables */
      if (not_initialized_as_it_should_p (dim))
	{
	  ERROR_VARIABLE_NOT_INITIALIZED (TREE_PURPOSE (cdim), 
					  DECL_NAME (dim));
	  INITIALIZED_P (dim) = 1;
	  error_found = 1;
	}

      TREE_PURPOSE (cdim) = NULL_TREE;
    }

  /* Resolve array base type if unresolved */
  if (!(type = resolve_type_during_patch (type)))
    error_found = 1;

  if (error_found)
    {
      /* We don't want further evaluation of this bogus array creation
         operation */
      TREE_TYPE (node) = error_mark_node;
      return error_mark_node;
    }

  /* The node is transformed into a function call. Things are done
     differently according to the number of dimensions. If the number
     of dimension is equal to 1, then the nature of the base type
     (primitive or not) matters. */
  total_dims = xdims + ndims;
  if (total_dims == 1)
    {
      if (JPRIMITIVE_TYPE_P (type))
	{
	  int type_code;
	  if (type == boolean_type_node)
	    type_code = 4;
	  else if (type == char_type_node)
	    type_code = 5;
	  else if (type == float_type_node)
	    type_code = 6;
	  else if (type == double_type_node)
	    type_code = 7;
	  else if (type == byte_type_node)
	    type_code = 8;
	  else if (type == short_type_node)
	    type_code = 9;
	  else if (type == int_type_node)
	    type_code = 10;
	  else if (type == long_type_node)
	    type_code = 11;
	  else
	    fatal ("Can't compute type code - patch_newarray");
	  return build_newarray (type_code, TREE_VALUE (dims));
	}
      else
	return build_anewarray (type, TREE_VALUE (dims));
    }
  
  /* Add extra dimensions as unknown dimensions */
  while (xdims--)
    dims = 
      chainon (dims, build_tree_list (NULL_TREE, integer_negative_one_node));
  dims = chainon (dims, build_tree_list (NULL_TREE, integer_zero_node));

  /* Can't reuse what's already written in expr.c because it uses the
     JVM stack representation. Provide a build_multianewarray. FIXME */
  array_type = type;
  for (cdim = TREE_CHAIN (dims); cdim; cdim = TREE_CHAIN (cdim))
    array_type = build_java_array_type (promote_type (array_type), 
					TREE_CODE (cdim) == INTEGER_CST ?
					TREE_INT_CST_LOW (cdim) : -1);
  return build (CALL_EXPR,
		promote_type (array_type),
		build_address_of (soft_multianewarray_node),
		tree_cons (NULL_TREE, build_class_ref (array_type),
			   tree_cons (NULL_TREE, 
				      build_int_2 (total_dims, 0), dims )),
		NULL_TREE);
}

static tree
build_this (location)
     int location;
{
  tree node = build_wfl_node (this_identifier_node, input_filename, 0, 0);
  TREE_SET_CODE (node, JAVA_THIS_EXPR);
  EXPR_WFL_LINECOL (node) = location;
  return node;
}

/* 14.15 The return statement. It builds a modify expression that
   assigns the returned value to the RESULT_DECL that hold the value
   to be returned. */

static tree
build_return (location, op)
     int location;
     tree op;
{
  tree node = build1 (RETURN_EXPR, NULL_TREE, op);
  EXPR_WFL_LINECOL (node) = location;
  return node;
}

static tree
patch_return (node)
     tree node;
{
  tree return_exp = TREE_OPERAND (node, 0);
  tree meth = current_function_decl;
  tree mtype = TREE_TYPE (TREE_TYPE (current_function_decl));
  tree modify;
  int error_found = 0;

  TREE_TYPE (node) = error_mark_node;
  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  /* It's invalid to have a return value within a function that is
     declared with the keyword void or that is a constructor */
  if (return_exp && (mtype == void_type_node || DECL_CONSTRUCTOR_P (meth)))
    error_found = 1;

  /* It's invalid to have a no return value within a function that
     isn't declared with the keyword `void' */
  if (!return_exp && (mtype != void_type_node && !DECL_CONSTRUCTOR_P (meth)))
    error_found = 2;

  if (error_found)
    {
      char *t = strdup ((char *)lang_printable_name (mtype));
      parse_error_context (wfl_operator, "`return' with%s value from `%s %s'",
			   (error_found == 1 ? "" : "out"), t,
			   lang_printable_name (meth));
      free (t);
      return error_mark_node;
    }

  /* If we have a return_exp, build a modify expression and expand it */
  if (return_exp)
    {
      modify = build (MODIFY_EXPR, NULL_TREE, DECL_RESULT (meth), return_exp);
      EXPR_WFL_LINECOL (modify) = EXPR_WFL_LINECOL (node);
      modify = java_complete_tree (modify);
      if (modify != error_mark_node)
	{
	  TREE_SIDE_EFFECTS (modify) = 1;
	  TREE_OPERAND (node, 0) = modify;
	}
      else
	return error_mark_node;
    }
  TREE_TYPE (node) = void_type_node;
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* 14.8 The if Statement */

static tree
build_if_else_statement (location, expression, if_body, else_body)
     int location;
     tree expression, if_body, else_body;
{
  tree node;
  /* FIXME: make else body be a void node, where this function is
     called */
  if (!else_body)
    else_body = build (COMPOUND_EXPR, void_type_node, NULL_TREE, NULL_TREE);
  node = build (COND_EXPR, NULL_TREE, expression, if_body, else_body);
  EXPR_WFL_LINECOL (node) = location;
  return node;
}

static tree
patch_if_else_statement (node)
     tree node;
{
  tree expression = TREE_OPERAND (node, 0);

  TREE_TYPE (node) = error_mark_node;
  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  /* The type of expression must be boolean */
  if (TREE_TYPE (expression) != boolean_type_node)
    {
      parse_error_context 
	(wfl_operator, 
	 "Incompatible type for `if'. Can't convert `%s' to `boolean'", 
	 lang_printable_name (TREE_TYPE (expression)));
      return error_mark_node;
    }
  
  TREE_TYPE (node) = void_type_node;
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* 14.6 Labeled Statements */

/* Action taken when a lableled statement is parsed. a new
   LABELED_BLOCK_EXPR is created. No statement is attached to the
   label, yet.  */

static tree
build_labeled_block (location, label, wfl)
     int location;
     tree label, wfl;
{
  tree label_name = merge_qualified_name (label_id, label);
  tree label_decl, node;

  /* Issue a warning if we try to reuse a label that was previously
     declared */
  if (IDENTIFIER_LOCAL_VALUE (label_name))
    {
      EXPR_WFL_LINECOL (wfl_operator) = location;
      parse_warning_context (wfl_operator, "Declaration of `%s' shadows "
			     "a previous declaration",
			     IDENTIFIER_POINTER (label));
      EXPR_WFL_LINECOL (wfl_operator) = 
        EXPR_WFL_LINECOL (IDENTIFIER_LOCAL_VALUE (label_name));
      parse_warning_context (wfl_operator, "This is the location of the "
			     "previous declaration of label `%s'",
			     IDENTIFIER_POINTER (label));
      java_warning_count--;
    }

  label_decl = create_label_decl (label_name);
  node = build (LABELED_BLOCK_EXPR, NULL_TREE, label_decl, NULL_TREE);
  EXPR_WFL_LINECOL (node) = location;
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* Generate a label crafting a unique name for it. This is used to
   implicitely label loops that aren't the body part of labeled
   statement.  */

static tree
generate_labeled_block ()
{
  static int l_number = 0;
  char buf [20];
  tree label_name;
  
  sprintf (buf, "$a%d", l_number++);
  return build_labeled_block (0, get_identifier (buf), NULL_TREE);
}

/* A labeled statement LBE is attached a statement. If the statement
   happens to be a loop, a link from the loop back to the label is
   installed.  */

static tree
complete_labeled_statement (lbe, statement)
     tree lbe;			/* Labeled block expr */
     tree statement;
{
  /* In anyways, tie the loop to its statement */
  LABELED_BLOCK_BODY (lbe) = statement;

  /* Ok, if statement is a for loop, we have to attach the labeled
     statement to the block the for loop belongs to and return the
     block instead */
  if (TREE_CODE (statement) == LOOP_EXPR && IS_FOR_LOOP_P (statement))
    {
      java_method_add_stmt (current_function_decl, lbe);
      return exit_block ();
    }

  return lbe;
}

/* 14.10, 14.11, 14.12 Loop Statements */

/* Create an empty LOOP_EXPR and make it the last in the nested loop
   list. */

static tree
build_new_loop (loop_body)
     tree loop_body;
{
  tree loop =  build (LOOP_EXPR, NULL_TREE, loop_body);
  TREE_SIDE_EFFECTS (loop) = 1;
  PUSH_LOOP (loop);
  return loop;
}

/* Create a loop body according to the following structure:
     COMPOUND_EXPR
       COMPOUND_EXPR		(loop main body)
         EXIT_EXPR		(this order is for while/for loops.
         LABELED_BLOCK_EXPR      the order is reversed for do loops)
           LABEL_DECL           (continue occurding here branche at the 
           BODY			 end of this labeled block)
       INCREMENT		(if any)

  REVERSED, if non zero, tells that the loop condition expr comes
  after the body, like in the do-while loop.  */

static tree
build_loop_body (location, condition, reversed)
     int location;
     tree condition;
     int reversed;
{
  tree first, second, label, body;

  condition = build (EXIT_EXPR, NULL_TREE, condition); /* Force walk */
  EXPR_WFL_LINECOL (condition) = location; /* For accurate error report */
  condition = build_debugable_stmt (location, condition);
  TREE_SIDE_EFFECTS (condition) = 1;

  body = generate_labeled_block ();
  first = (reversed ? body : condition);
  second = (reversed ? condition : body);
  return 
    build (COMPOUND_EXPR, NULL_TREE, 
	   build (COMPOUND_EXPR, NULL_TREE, first, second), size_zero_node);
}

/* Install CONDITION (if any) and loop BODY (using REVERSED to tell
   their order) on the current loop. Unlink the current loop from the
   loop list.  */

static tree
complete_loop_body (location, condition, body, reversed)
     int location;
     tree condition, body;
     int reversed;
{
  tree to_return = ctxp->current_loop;
  tree loop_body = LOOP_EXPR_BODY (to_return);
  if (condition)
    {
      tree cnode = LOOP_EXPR_BODY_CONDITION_EXPR (loop_body, reversed);
      /* We wrapped the EXIT_EXPR around a WFL so we can debug it.
         The real EXIT_EXPR is one operand further. */
      EXPR_WFL_LINECOL (cnode) = location;
      /* This one is for accurate error reports */
      EXPR_WFL_LINECOL (TREE_OPERAND (cnode, 0)) = location;
      TREE_OPERAND (TREE_OPERAND (cnode, 0), 0) = condition;
    }
  LOOP_EXPR_BODY_BODY_EXPR (loop_body, reversed) = body;
  POP_LOOP ();
  return to_return;
}

/* Tailored version of complete_loop_body for FOR loops, when FOR
   loops feature the condition part */

static tree
complete_for_loop (location, condition, update, body)
    int location;
    tree condition, update, body;
{
  /* Put the condition and the loop body in place */
  tree loop = complete_loop_body (location, condition, body, 0);
  /* LOOP is the current loop which has been now popped of the loop
     stack. Install the update block */
  LOOP_EXPR_BODY_UPDATE_BLOCK (LOOP_EXPR_BODY (loop)) = update;
  return loop;
}

/* If the loop isn't surrounded by a labeled statement, create one and
   insert LOOP as it's body.  */

static tree
patch_loop_statement (loop)
     tree loop;
{
  tree cbl, loop_label, to_return_as_loop;

  if (LOOP_HAS_LABEL_P (loop))
    {
      loop_label = ctxp->current_labeled_block;
      to_return_as_loop = loop;
    }
  else
    {
      loop_label = generate_labeled_block ();
      LABELED_BLOCK_BODY (loop_label) = loop;
      PUSH_LABELED_BLOCK (loop_label);
      to_return_as_loop = loop_label;
    }
  TREE_TYPE (to_return_as_loop) = void_type_node;
  return to_return_as_loop;
}

/* 14.13, 14.14: break and continue Statements */

/* Build a break or a continue statement. a null NAME indicates an
   unlabeled break/continue statement.  */

static tree
build_bc_statement (location, is_break, name)
     int location, is_break;
     tree name;
{
  tree break_continue, label_block_expr = NULL_TREE;

  if (name)
    {
      if (!(label_block_expr = IDENTIFIER_LOCAL_VALUE 
	    (merge_qualified_name (label_id, EXPR_WFL_NODE (name)))))
	/* Null means that we don't have a target for this named
	   break/continue. In this case, we make the target to be the
	   label name, so that the error can be reported accuratly in
	   patch_bc_statement. */
	label_block_expr = EXPR_WFL_NODE (name);
    }
  /* Unlabeled break/continue will be handled during the
     break/continue patch operation */
  break_continue 
    = build (EXIT_BLOCK_EXPR, NULL_TREE, label_block_expr, NULL_TREE);

  IS_BREAK_STMT_P (break_continue) = is_break;
  TREE_SIDE_EFFECTS (break_continue) = 1;
  EXPR_WFL_LINECOL (break_continue) = location;
  return break_continue;
}

/* Verification of a break/continue statement. */

static tree
patch_bc_statement (node)
     tree node;
{
  tree bc_label = EXIT_BLOCK_LABELED_BLOCK (node), target_stmt;
  int is_unlabeled = 0;
 EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);
 
  /* Not having a target means that the break/continue statement is
     unlabeled. We try to find a descent label for it */
  if (!bc_label)
    {
      is_unlabeled = 1;
      /* There should be a loop to branch to */
      if (ctxp->current_loop)
	{
	  /* At that stage, we're in the loop body, which is
	     encapsulated around a LABELED_BLOCK_EXPR. So searching
	     the current loop label requires us to consider the
	     labeled block before the current one. */
	  if (!LOOP_HAS_LABEL_SKIP_P (ctxp->current_loop))
	    fatal ("unlabeled loop has not installed label -- "
		   "patch_bc_statement");
	  bc_label = TREE_CHAIN (ctxp->current_labeled_block);
	}
      /* Not having a loop to break/continue to is an error */
      else
	{
	  parse_error_context (wfl_operator, "`%s' must be in loop%s",
			       (IS_BREAK_STMT_P (node) ? "break" : "continue"),
			       (IS_BREAK_STMT_P (node) ? " or switch" : ""));
	  return error_mark_node;
	}
    }
  /* Having an identifier here means that the target is unknown. */
  else if (TREE_CODE (bc_label) == IDENTIFIER_NODE)
    {
      parse_error_context (wfl_operator, "No label definition found for `%s'",
			   IDENTIFIER_POINTER (bc_label));
      return error_mark_node;
    }

  /* Find the statement we're targeting. */
  target_stmt = LABELED_BLOCK_BODY (bc_label);

  /* 14.13 The break Statement */
  if (IS_BREAK_STMT_P (node))
    {
      /* Named break are always fine, as far as they have a target
         (already verified). Anonymous break need to target
         while/do/for/switch */
      if (is_unlabeled &&
	  !(TREE_CODE (target_stmt) == LOOP_EXPR   /* do/while/for */
	    || 0))		                   /* switch FIXME */
	{
	  parse_error_context (wfl_operator, 
			       "`break' must be in loop or switch");
	  return error_mark_node;
	}
      /* If previously unlabeled, install the new found label */
      if (is_unlabeled)
	EXIT_BLOCK_LABELED_BLOCK (node) = bc_label;
    }
  /* 14.14 The continue Statement */
  /* The continue statement must always target a loop */
  else 
    {
      if (TREE_CODE (target_stmt) != LOOP_EXPR) /* do/while/for */
	{
	  parse_error_context (wfl_operator, "`continue' must be in loop");
	  return error_mark_node;
	}
      /* Everything looks good. We can fix the `continue' jump to go
         at the place in the loop were the continue is. The continue
	 is the current labeled block, by construction. */
      EXIT_BLOCK_LABELED_BLOCK (node) = ctxp->current_labeled_block;
    }

  /* Our break/continue don't return values. */
  TREE_TYPE (node) = void_type_node;
  /* Encapsulate the break within a compound statement so that it's
     expanded all the times by expand_expr (and not clobered
     sometimes, like after a if statement) */
  node = add_stmt_to_compound (NULL_TREE, void_type_node, node);
  TREE_SIDE_EFFECTS (node) = 1;
  return node;
}

/* Process the exit expression belonging to a loop. Its type must be
   boolean.  */

static tree
patch_exit_expr (node)
     tree node;
{
  tree expression = TREE_OPERAND (node, 0);
  TREE_TYPE (node) = error_mark_node;
  EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (node);

  /* The type of expression must be boolean */
  if (TREE_TYPE (expression) != boolean_type_node)
    {
      parse_error_context 
	(wfl_operator, 
	 "Incompatible type for loop conditional. Can't convert `%s' to "
	 "`boolean'", 
	 lang_printable_name (TREE_TYPE (expression)));
      return error_mark_node;
    }
  /* Now we know things are allright, invert the condition, fold and
     return */
  TREE_OPERAND (node, 0) = 
    fold (build1 (TRUTH_NOT_EXPR, boolean_type_node, expression));
  TREE_TYPE (node) = void_type_node;
  return node;
}
