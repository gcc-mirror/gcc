/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dN -M" } */

/* Test -dN -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dN-M.i] { return }                } }
   { dg-final { if { [grep cmdlne-dN-M.i "^#define foo"] != "" } { fail "cmdlne-dN-M.c: #define line printed" } } }
   { dg-final { if { [grep cmdlne-dN-M.i "variable"] != "" } { fail "cmdlne-dN-M.c: non-#define line printed" } } }
   { dg-final { if { [grep cmdlne-dN-M.i "^cmdlne-dN-M.*:.*cmdlne-dN-M.c"] == "" } { xfail "cmdlne-dN-M.c: dependency rule not printed" } } }
   { dg-final { return } }  */
