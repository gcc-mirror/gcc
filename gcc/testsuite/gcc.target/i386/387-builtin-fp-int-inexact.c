/* Test -fno-fp-int-builtin-inexact for 387.  */
/* { dg-do run } */
/* { dg-options "-O2 -mfancy-math-387 -mfpmath=387 -fno-fp-int-builtin-inexact" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#include "../../gcc.dg/torture/builtin-fp-int-inexact.c"
