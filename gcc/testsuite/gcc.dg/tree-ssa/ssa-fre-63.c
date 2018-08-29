/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-stats" } */

int foo(char *x)
{
  __builtin_memset (&x[1], 'c', 42);
  return x[0] + x[1] + x[42] + x[43];
}

/* We should eliminate x[1] and x[42] and their conversions to int.  */
/* { dg-final { scan-tree-dump "Eliminated: 4" "fre1" } } */
