/* Copyright (C) 2002, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dM -dD" } */

/* Test -dM -dD does not fail.  It should give the same output
   as plain -dD.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { scan-file cmdlne-dM-dD.i "(^|\\n)#define foo bar($|\\n)" } }
   { dg-final { scan-file cmdlne-dM-dD.i "variable" } } */
