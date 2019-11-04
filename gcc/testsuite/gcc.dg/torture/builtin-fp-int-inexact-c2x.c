/* Test C2X enables -fno-fp-int-builtin-inexact.  */
/* { dg-do run } */
/* { dg-options "-std=c2x" } */
/* { dg-require-effective-target fenv_exceptions } */

#include "builtin-fp-int-inexact.c"
