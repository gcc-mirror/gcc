/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -fdump-tree-optimized" } */

/* PR tree-optimization/117234  */
/* PAREN_EXPR should not be declared as trapping.  */

float f1(float a)
{
  try {
    return __builtin_assoc_barrier (a);
  }  catch(...)
  { __builtin_trap (); }
}

/* { dg-final { scan-tree-dump-not "__builtin_trap" "optimized" } } */
