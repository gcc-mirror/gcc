/* Copyright (C) 2002, 2003, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dN -M" } */

/* Test -dN -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file-not cmdlne-dN-M.i "(^|\\n)#define foo" } }
   { dg-final { scan-file-not cmdlne-dN-M.i "variable" } }
   { dg-final { scan-file cmdlne-dN-M.i "(^|\\n)cmdlne-dN-M\[^\\n\]*:( *\\\\\\n)?\[^\\n\]*cmdlne-dN-M.c" } } */
