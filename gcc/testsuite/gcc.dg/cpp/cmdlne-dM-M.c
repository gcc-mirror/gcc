/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dM -M" } */

/* Test -dM -M does not fail.  It should print both the
   #define lines and a Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dM-M.i] { return }                } }
   { dg-final { if { [grep cmdlne-dM-M.i "^#define foo bar$"] == "" } { fail "cmdlne-dM-M.c: #define line not printed" } } }
   { dg-final { if { [grep cmdlne-dM-M.i "variable"] != "" } { fail "cmdlne-dM-M.c: non-#define line printed" } } }
   { dg-final { if { [grep cmdlne-dM-M.i "^cmdlne-dM-M.*:.*cmdlne-dM-M.c"] == "" } { xfail "cmdlne-dM-M.c: dependency rule not printed" } } }
   { dg-final { return } }  */
