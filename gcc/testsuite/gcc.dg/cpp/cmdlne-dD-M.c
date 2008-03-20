/* Copyright (C) 2002, 2003, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dD -M" } */

/* Test -dD -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file-not cmdlne-dD-M.i "(^|\\n)#define foo bar($|\\n)" } }
   { dg-final { scan-file-not cmdlne-dD-M.i "variable" } }
   { dg-final { scan-file cmdlne-dD-M.i "(^|\n)cmdlne-dD-M\[^\\n\]*:( *\\\\\\n)?\[^\\n\]*cmdlne-dD-M.c" } } */
