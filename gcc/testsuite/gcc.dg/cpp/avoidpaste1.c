/* Copyright (C) 2001, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This tests that we avoid accidental pasting only before and after
   macros and arguments, and not when the tokens are already pasted
   in the souce file (e.g. "::" in a C source file).

   Neil Booth, 28 Jan 2001.  */

#define f(x) x
#define g
#define tricky 1.0e ## -1

/* This should preprocess as

:: : : : : :^: 1.0e- 1
: : : .. . 0 0 .

It relies on the fact that even when preprocessing C we bother to separate
the colons of C++'s :: operator.  If we confine this behavior to C++
in future, this test needs to change.  */

:: :g: :f(): :f(^): tricky
:f(:): .. .__INCLUDE_LEVEL__ __INCLUDE_LEVEL__. /* Check builtins, too.  */

/* { dg-final { scan-file avoidpaste1.i ":: : : : : :\\^: 1.0e- 1" } }
   { dg-final { scan-file avoidpaste1.i ": : : \\\.\\\. \\\. 0 0 \\\." } } */
