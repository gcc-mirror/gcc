/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-C" } */

/* This tests that C++ comments are either dropped, or converted to C
   comments in macro expansions.  The + in the regexp stops it from
   matching itself 8-)

   Neil Booth, 9 Oct 2001.  */

#define ZERO 0 // A comment

ZERO:

/*
   { dg-final { if ![file exists cmdlne-C2.i] { return }                  } }
   { dg-final { if { [grep cmdlne-C2.i "c+omment:"] == "" } { return }    } }
   { dg-final { fail "cmdlne-C2.i: C++ comments in macros with -C"        } }
*/

