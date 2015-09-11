/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

struct A { char c[4]; } a, b;

void
f1 (void)
{
  __builtin_memcpy (&a.c[0], "a", 1);
  a = b;
}

void
f2 (void)
{
  __builtin_memcpy (&a.c[0], "a", 1);
  __builtin_memcpy (&a.c[0], "cdef", 4);
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1" } } */
