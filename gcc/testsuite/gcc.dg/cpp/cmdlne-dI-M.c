/* Copyright (C) 2002, 2003, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dI -M" } */

/* Test -dI -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#include "cmdlne-dI-M.h"
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file-not cmdlne-dI-M.i "(^|\\n)#define foo bar($|\\n)" } }
   { dg-final { scan-file-not cmdlne-dI-M.i "variable" } }
   { dg-final { scan-file cmdlne-dI-M.i "(^|\\n)cmdlne-dI-M\[^\\n\]*:( *\\\\\\n)?\[^\\n\]*cmdlne-dI-M.c" } } */
