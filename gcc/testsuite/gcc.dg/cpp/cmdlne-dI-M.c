/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-dI -M" } */

/* Test -dI -M does not fail.  It should print just
   the Makefile rule with dependencies.  */

#define foo bar
#include "cmdlne-dI-M.h"
#define funlike(like) fun like
int variable;

/* { dg-final { if ![file exists cmdlne-dI-M.i] { return }                } }
   { dg-final { if { [grep cmdlne-dI-M.i "^#define foo bar$"] != "" } { fail "cmdlne-dI-M.c: #define line printed" } } }
   { dg-final { if { [grep cmdlne-dI-M.i "variable"] != "" } { fail "cmdlne-dI-M.c: non-#define line printed" } } }
   { dg-final { if { [grep cmdlne-dI-M.i "^cmdlne-dI-M.*:.*cmdlne-dI-M.c"] == "" } { xfail "cmdlne-dI-M.c: dependency rule not printed" } } }
   { dg-final { return } }  */
