/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dM -dD" } */

/* Test -dM -dD does not fail.  It should give the same output
   as plain -dD.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dM-dD.i] { return }                } }
   { dg-final { if { [grep cmdlne-dM-dD.i "^#define foo bar$"] == "" } { fail "cmdlne-dM-dD.c: #define line not printed" } } }
   { dg-final { if { [grep cmdlne-dM-dD.i "variable"] == "" } { fail "cmdlne-dM-dD.c: non-#define line not printed" } } }
   { dg-final { return } }  */
