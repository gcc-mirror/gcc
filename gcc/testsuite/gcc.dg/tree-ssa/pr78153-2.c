/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-slim" } */

void f(const char *s)
{
  __PTRDIFF_TYPE__ n = __builtin_strlen (s);
  if (n < 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "__builtin_abort" "evrp" } } */
