/* { dg-do compile { target { powerpc-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Based on builtins-1-le.c ; ensure that the power8 builtins are accepted by
   the compiler, at O2 with gimple folding enabled.  */
/* Test that a number of newly added builtin overloads are accepted
   by the compiler.  */

/* The test code is in builtins-1.fold.h.  */
#include "builtins-1.fold.h"
