/* -*- indented-text -*- */
/* Process source files and output type information.
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

%{
#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gengtype.h"
#define YYERROR_VERBOSE
%}

%union {
  type_p t;
  pair_p p;
  options_p o;
  const char *s;
}

%token <s>ENT_TYPEDEF_STRUCT
%token <s>ENT_STRUCT
%token <s>ENT_TYPEDEF_UNION
%token <s>ENT_UNION
%token ENT_EXTERNSTATIC
%token GTY_TOKEN
%token VEC_TOKEN
%token UNION
%token STRUCT
%token ENUM
%token ALIAS
%token NESTED_PTR
%token <s>PARAM_IS
%token NUM
%token <s>SCALAR
%token <s>ID
%token <s>STRING
%token <s>ARRAY
%token <s>CHAR

%type <p> struct_fields
%type <t> type lasttype
%type <o> optionsopt options optionseq
%type <s> type_option stringseq

%%

start: /* empty */
       | typedef_struct start
       | externstatic start
       | start
       ;

typedef_struct: ENT_TYPEDEF_STRUCT options '{' struct_fields '}' ID
		   {
		     type_p t = new_structure ($1, false, &lexer_line, $4, $2);
		     do_typedef ($6, t, &lexer_line);
		     lexer_toplevel_done = 1;
		   }
		  ';'
		| ENT_TYPEDEF_UNION options '{' struct_fields '}' ID
		   {
		     type_p t = new_structure ($1, true, &lexer_line, $4, $2);
		     do_typedef ($6, t, &lexer_line);
		     lexer_toplevel_done = 1;
		   }
		  ';'
		| ENT_STRUCT options '{' struct_fields '}'
		   {
		     new_structure ($1, false, &lexer_line, $4, $2);
		     lexer_toplevel_done = 1;
		   }
		  ';'
		| ENT_UNION options '{' struct_fields '}'
		   {
		     new_structure ($1, true, &lexer_line, $4, $2);
		     lexer_toplevel_done = 1;
		   }
		  ';'
		;

externstatic: ENT_EXTERNSTATIC options lasttype ID semiequal
	         {
	           note_variable ($4, adjust_field_type ($3, $2), $2,
				  &lexer_line);
	         }
	      | ENT_EXTERNSTATIC options lasttype ID ARRAY semiequal
	         {
	           note_variable ($4, create_array ($3, $5),
	      		    $2, &lexer_line);
	         }
	      | ENT_EXTERNSTATIC options lasttype ID ARRAY ARRAY semiequal
	         {
	           note_variable ($4, create_array (create_array ($3, $6),
	      				      $5),
	      		    $2, &lexer_line);
	         }
	      ;

lasttype: type
	    {
	      lexer_toplevel_done = 1;
	      $$ = $1;
	    }
	    ;

semiequal: ';'
	   | '='
	   ;

struct_fields: { $$ = NULL; }
	       | type optionsopt ID bitfieldopt ';' struct_fields
	          {
		    $$ = create_field_at ($6, $1, $3, $2, &lexer_line);
		  }
	       | type optionsopt ID ARRAY ';' struct_fields
	          {
		    $$ = create_field_at ($6, create_array ($1, $4),
		    			  $3, $2, &lexer_line);
		  }
	       | type optionsopt ID ARRAY ARRAY ';' struct_fields
	          {
		    type_p arr = create_array (create_array ($1, $5), $4);
		    $$ = create_field_at ($7, arr, $3, $2, &lexer_line);
		  }
	       | type ':' bitfieldlen ';' struct_fields
		  { $$ = $5; }
	       ;

bitfieldopt: /* empty */
	     | ':' bitfieldlen
	     ;

bitfieldlen: NUM | ID
		{ }
	     ;

type: SCALAR
         { $$ = create_scalar_type ($1); }
      | ID
         { $$ = resolve_typedef ($1, &lexer_line); }
      | VEC_TOKEN '(' ID ',' ID ')'
         { $$ = resolve_typedef (concat ("VEC_", $3, "_", $5, (char *)0),
	      			 &lexer_line); }
      | type '*'
         { $$ = create_pointer ($1); }
      | STRUCT ID '{' struct_fields '}'
         { $$ = new_structure ($2, 0, &lexer_line, $4, NULL); }
      | STRUCT ID
         { $$ = find_structure ($2, 0); }
      | UNION ID '{' struct_fields '}'
         { $$ = new_structure ($2, 1, &lexer_line, $4, NULL); }
      | UNION ID
         { $$ = find_structure ($2, 1); }
      | ENUM ID
         { $$ = create_scalar_type ($2); }
      | ENUM ID '{' enum_items '}'
         { $$ = create_scalar_type ($2); }
      ;

enum_items: /* empty */
	    | ID '=' NUM ',' enum_items
	      { }
	    | ID ',' enum_items
	      { }
	    | ID enum_items
	      { }
	    ;

optionsopt: { $$ = NULL; }
	    | options { $$ = $1; }
	    ;

options: GTY_TOKEN '(' '(' optionseq ')' ')'
	   { $$ = $4; }
	 ;

type_option : ALIAS
	        { $$ = "ptr_alias"; }
	      | PARAM_IS
	        { $$ = $1; }
	      ;

optionseq: { $$ = NULL; }
	| optionseq commaopt ID
	   { $$ = create_option ($1, $3, (void *)""); }
	| optionseq commaopt ID '(' stringseq ')'
	   { $$ = create_option ($1, $3, (void *)$5); }
	| optionseq commaopt type_option '(' type ')'
	   { $$ = create_option ($1, $3, adjust_field_type ($5, 0)); }
	| optionseq commaopt NESTED_PTR '(' type ',' stringseq ',' stringseq ')'
	   { $$ = create_nested_ptr_option ($1, $5, $7, $9); }

commaopt: /* nothing */
	  | ','
	  ;

stringseq: STRING
	     { $$ = $1; }
	   | stringseq STRING
	     {
	       size_t l1 = strlen ($1);
	       size_t l2 = strlen ($2);
	       char *s = XRESIZEVEC (char, $1, l1 + l2 + 1);
	       memcpy (s + l1, $2, l2 + 1);
	       XDELETE ($2);
	       $$ = s;
	     }
	   ;
%%
