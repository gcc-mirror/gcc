/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-C -traditional-cpp" } */

/* Test that comments are actually written out

   Neil Booth, 24 Jun 2002.  */

/*
   { dg-final { if ![file exists cmdlne-C2.i] { return }                  } }
   { dg-final { if { [grep cmdlne-C2.i "dg-final"] != "" } { return }    } }
   { dg-final { fail "cmdlne-C2.i: C comments output with -C"        } }
*/

