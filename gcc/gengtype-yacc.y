/* -*- indented-text -*- */
/* Process source files and output type information.
   Copyright (C) 2002 Free Software Foundation, Inc.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

%{
#include "hconfig.h"
#include "system.h"
#include "gengtype.h"
#define YYERROR_VERBOSE
%}

%union {
  type_p t;
  pair_p p;
  options_p o;
  const char *s;
}

%token <t>ENT_TYPEDEF_STRUCT
%token <t>ENT_STRUCT
%token ENT_EXTERNSTATIC
%token ENT_YACCUNION
%token GTY_TOKEN
%token UNION
%token STRUCT
%token ENUM
%token ALIAS
%token <s>PARAM_IS
%token NUM
%token PERCENTPERCENT "%%"
%token <t>SCALAR
%token <s>ID
%token <s>STRING
%token <s>ARRAY
%token <s>PERCENT_ID
%token <s>CHAR

%type <p> struct_fields yacc_ids yacc_typematch
%type <t> type lasttype
%type <o> optionsopt options option optionseq optionseqopt
%type <s> type_option

%%

start: /* empty */
       | typedef_struct start
       | externstatic start
       | yacc_union start
       ;

typedef_struct: ENT_TYPEDEF_STRUCT options '{' struct_fields '}' ID
		   {
		     new_structure ($1->u.s.tag, UNION_P ($1), &lexer_line,
				    $4, $2);
		     do_typedef ($6, $1, &lexer_line);
		     lexer_toplevel_done = 1;
		   }
		 ';'
		   {}
		| ENT_STRUCT options '{' struct_fields '}'
		   {
		     new_structure ($1->u.s.tag, UNION_P ($1), &lexer_line,
				    $4, $2);
		     lexer_toplevel_done = 1;
		   }
		 ';'
		   {}
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

yacc_union: ENT_YACCUNION options struct_fields '}' yacc_typematch
	    PERCENTPERCENT
	      {
	        note_yacc_type ($2, $3, $5, &lexer_line);
	      }
	    ;

yacc_typematch: /* empty */
		   { $$ = NULL; }
		| yacc_typematch PERCENT_ID yacc_ids
		   {
		     pair_p p;
		     for (p = $3; p->next != NULL; p = p->next)
		       {
		         p->name = NULL;
			 p->type = NULL;
		       }
		     p->name = NULL;
		     p->type = NULL;
		     p->next = $1;
		     $$ = $3;
		   }
		| yacc_typematch PERCENT_ID '<' ID '>' yacc_ids
		   {
		     pair_p p;
		     type_p newtype = NULL;
		     if (strcmp ($2, "type") == 0)
		       newtype = (type_p) 1;
		     for (p = $6; p->next != NULL; p = p->next)
		       {
		         p->name = $4;
		         p->type = newtype;
		       }
		     p->name = $4;
		     p->next = $1;
		     p->type = newtype;
		     $$ = $6;
		   }
		;

yacc_ids: /* empty */
	{ $$ = NULL; }
     | yacc_ids ID
        {
	  pair_p p = xcalloc (1, sizeof (*p));
	  p->next = $1;
	  p->line = lexer_line;
	  p->opt = xmalloc (sizeof (*(p->opt)));
	  p->opt->name = "tag";
	  p->opt->next = NULL;
	  p->opt->info = (char *)$2;
	  $$ = p;
	}
     | yacc_ids CHAR
        {
	  pair_p p = xcalloc (1, sizeof (*p));
	  p->next = $1;
	  p->line = lexer_line;
	  p->opt = xmalloc (sizeof (*(p->opt)));
	  p->opt->name = "tag";
	  p->opt->next = NULL;
	  p->opt->info = xasprintf ("'%s'", $2);
	  $$ = p;
	}
     ;

struct_fields: { $$ = NULL; }
	       | type optionsopt ID bitfieldopt ';' struct_fields
	          {
	            pair_p p = xmalloc (sizeof (*p));
		    p->type = adjust_field_type ($1, $2);
		    p->opt = $2;
		    p->name = $3;
		    p->next = $6;
		    p->line = lexer_line;
		    $$ = p;
		  }
	       | type optionsopt ID ARRAY ';' struct_fields
	          {
	            pair_p p = xmalloc (sizeof (*p));
		    p->type = adjust_field_type (create_array ($1, $4), $2);
		    p->opt = $2;
		    p->name = $3;
		    p->next = $6;
		    p->line = lexer_line;
		    $$ = p;
		  }
	       | type optionsopt ID ARRAY ARRAY ';' struct_fields
	          {
	            pair_p p = xmalloc (sizeof (*p));
		    p->type = create_array (create_array ($1, $5), $4);
		    p->opt = $2;
		    p->name = $3;
		    p->next = $7;
		    p->line = lexer_line;
		    $$ = p;
		  }
	       ;

bitfieldopt: /* empty */
	     | ':' NUM
	     | ':' ID
	     ;

type: SCALAR
         { $$ = $1; }
      | ID
         { $$ = resolve_typedef ($1, &lexer_line); }
      | type '*'
         { $$ = create_pointer ($1); }
      | STRUCT ID '{' struct_fields '}'
         {
	   new_structure ($2, 0, &lexer_line, $4, NULL);
           $$ = find_structure ($2, 0);
	 }
      | STRUCT ID
         { $$ = find_structure ($2, 0); }
      | UNION ID '{' struct_fields '}'
         {
	   new_structure ($2, 1, &lexer_line, $4, NULL);
           $$ = find_structure ($2, 1);
	 }
      | UNION ID
         { $$ = find_structure ($2, 1); }
      | ENUM ID
         { $$ = create_scalar_type ($2, strlen ($2)); }
      | ENUM ID '{' enum_items '}'
         { $$ = create_scalar_type ($2, strlen ($2)); }
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

options: GTY_TOKEN '(' '(' optionseqopt ')' ')'
	   { $$ = $4; }
	 ;

type_option : ALIAS
	        { $$ = "ptr_alias"; }
	      | PARAM_IS
	        { $$ = $1; }
	      ;

option:	type_option '(' type ')'
	   {
	     options_p o = xmalloc (sizeof (*o));
	     o->name = $1;
	     o->info = adjust_field_type ($3, NULL);
	     $$ = o;
	   }
	| ID '(' STRING ')'
	   {
	     options_p o = xmalloc (sizeof (*o));
	     o->name = $1;
	     o->info = (void *)$3;
	     $$ = o;
	   }
	;

optionseq: option
	      {
	        $1->next = NULL;
		$$ = $1;
	      }
	    | optionseq ',' option
	      {
	        $3->next = $1;
		$$ = $3;
	      }
	    ;

optionseqopt: { $$ = NULL; }
	      | optionseq { $$ = $1; }
	      ;
%%
