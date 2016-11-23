/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-slim" } */

void f(const char *s)
{
  if (__PTRDIFF_MAX__ <= __builtin_strlen (s))
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "__builtin_abort" "evrp" } } */
