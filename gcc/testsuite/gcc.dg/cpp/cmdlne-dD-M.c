/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dD -M" } */

/* Test -dD -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dD-M.i] { return }                } }
   { dg-final { if { [grep cmdlne-dD-M.i "^#define foo bar$"] != "" } { fail "cmdlne-dD-M.c: #define line printed" } } }
   { dg-final { if { [grep cmdlne-dD-M.i "variable"] != "" } { fail "cmdlne-dD-M.c: non-#define line printed" } } }
   { dg-final { if { [grep cmdlne-dD-M.i "^cmdlne-dD-M.*:.*cmdlne-dD-M.c"] == "" } { xfail "cmdlne-dD-M.c: dependency rule not printed" } } }
   { dg-final { return } }  */
