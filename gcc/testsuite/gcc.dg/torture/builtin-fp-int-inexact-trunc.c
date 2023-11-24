/* Test -fno-fp-int-builtin-inexact.  */
/* { dg-do compile } */
/* { dg-options "-fno-fp-int-builtin-inexact -fdump-tree-original" } */

long
x (double y)
{
  return __builtin_trunc (y);
}

/* Optimization should not discard the __builtin_trunc call.  */
/* { dg-final { scan-tree-dump "__builtin_trunc" "original" } } */
