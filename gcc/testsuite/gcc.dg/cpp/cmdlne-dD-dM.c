/* Copyright (C) 2002, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dD -dM" } */

/* Test -dD -dM does not fail.  It should give the same output
   as plain -dM.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file cmdlne-dD-dM.i "(^|\\n)#define foo bar($|\\n)" } }
   { dg-final { scan-file-not cmdlne-dD-dM.i "variable" } } */
