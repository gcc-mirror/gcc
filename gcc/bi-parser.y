/* Bytecode definition file parser.
   Copyright (C) 1993 Free Software Foundation, Inc.

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


%{

#include <stdio.h>
#include "hconfig.h"
#include "bi-defs.h"

extern char yytext[];
extern int yyleng;


/* Chain of all defs built by the parser. */
struct def *defs;
int ndefs;

static struct node *makenode ();
static struct variation *makevar ();
static struct def *makedef ();

void yyerror ();

%}

%union
{
  char *string;
  struct def *def;
  struct variation *variation;
  struct node *node;
}

%token <string> DEFOP STRING
%type <string> opt_string
%type <def> defs def
%type <variation> variations variation
%type <node> list items item

%%

top: 
  defs
    { defs = $1; }
  ;

defs:
  def
  | defs def
    { $2->next = $1; $$ = $2; }
  ;

def:
  DEFOP '(' STRING ',' opt_string ',' '(' variations ')' ')'
    { $$ = makedef ($3, $5, $8); }
  ;

variations:
  variation
  | variations ',' variation
    { $3->next = $1; $$ = $3; }
  ;

variation:
  '(' opt_string ')'
    { $$ = makevar ($2, (struct node *) NULL, (struct node *) NULL, (struct node *) NULL); }
  | '(' opt_string ',' list ')'
    { $$ = makevar ($2, $4, (struct node *) NULL, (struct node *) NULL); }
  | '(' opt_string ',' list ',' list ')'
    { $$ = makevar ($2, $4, $6, (struct node *) NULL); }
  | '(' opt_string ',' list ',' list ',' list ')'
    { $$ = makevar ($2, $4, $6, $8); }
  ;

opt_string:
  /* empty */ { $$ = ""; }
  | STRING { $$ = $1; }
  ;

list:
  '(' items ')'
    { $$ = $2; }
  | /* empty */
    { $$ = NULL; }
  ;

items:
  item
  /* Note right recursion. */
  | item ',' items
    { $1->next = $3; $$ = $1; }
  ;

item:
  STRING
    { $$ = makenode ($1); }
  ;

%%

static struct node *
makenode (s)
     char *s;
{
  struct node *n;

  n = (struct node *) malloc (sizeof (struct node));
  n->text = s;
  n->next = NULL;
  return n;
}

static struct variation *
makevar (name, inputs, outputs, literals)
     char *name;
     struct node *inputs, *outputs, *literals;
{
  struct variation *v;

  v = (struct variation *) malloc (sizeof (struct variation));
  v->name = name;
  v->code = ndefs++;
  v->inputs = inputs;
  v->outputs = outputs;
  v->literals = literals;
  v->next = NULL;
  return v;
}

static struct def *
makedef (name, template, vars)
     char *name, *template;
     struct variation *vars;
{
  struct def *d;

  d = (struct def *) malloc (sizeof (struct def));
  d->basename = name;
  d->template = template;
  d->variations = vars;
  d->next = NULL;
  return d;
}

void
yyerror (s)
     char *s;
{
  fprintf (stderr, "syntax error in input\n");
  exit (FATAL_EXIT_CODE);
}
