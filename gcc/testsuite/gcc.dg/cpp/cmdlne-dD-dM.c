/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dD -dM" } */

/* Test -dD -dM does not fail.  It should give the same output
   as plain -dM.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dD-dM.i] { return }                } }
   { dg-final { if { [grep cmdlne-dD-dM.i "^#define foo bar$"] == "" } { fail "cmdlne-dD-dM.c: #define line not printed" } } }
   { dg-final { if { [grep cmdlne-dD-dM.i "variable"] != "" } { fail "cmdlne-dD-dM.c: non-#define line printed" } } }
   { dg-final { return } }  */
