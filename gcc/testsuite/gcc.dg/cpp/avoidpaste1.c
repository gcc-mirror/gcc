/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This tests that we avoid accidental pasting only before and after
   macros and arguments, and not when the tokens are already pasted
   in the souce file (e.g. "::" in a C source file).

   Neil Booth, 28 Jan 2001.  */

#define f(x) x
#define g

/* This should preprocess as

:: : : : : :^:
: : : .. . 0

It relies on the fact that even preprocessing C we bother to separate
the colons of C++'s :: operator.  If we confine this behaviour to C++
in future, this test needs to change.  */

:: :g: :f(): :f(^):
:f(:): .. .__INCLUDE_LEVEL__	/* Check builtins, too.  */

/*
   { dg-final { if ![file exists avoidpaste1.i] { return }                } }
   { dg-final { if { [grep avoidpaste1.i ":: : : : : :\\^:"] != "" } \{   } }
   { dg-final { if { [grep avoidpaste1.i ": : : \\\.\\\. \\\. 0"] != "" } \{  } }
   { dg-final { return \} \}                                              } }
   { dg-final { fail "avoidpaste1.c: paste avoidance"                     } }
*/
