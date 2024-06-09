/* Test C23 enables -fno-fp-int-builtin-inexact.  */
/* { dg-do run } */
/* { dg-options "-std=c23" } */
/* { dg-require-effective-target fenv_exceptions } */

#include "builtin-fp-int-inexact.c"
