/* Copyright (C) 2002, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dM -M" } */

/* Test -dM -M does not fail.  It should print both the
   #define lines and a Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file cmdlne-dM-M.i "(^|\\n)#define foo bar($|\\n)" } }
   { dg-final { scan-file-not cmdlne-dM-M.i "variable" } }
   { dg-final { scan-file cmdlne-dM-M.i "(^|\\n)cmdlne-dM-M\[^\\n\]*:\[^\\n\]*cmdlne-dM-M.c" { xfail *-*-* } } } */
