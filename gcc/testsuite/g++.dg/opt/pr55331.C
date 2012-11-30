// PR tree-optimization/55331
// { dg-do compile }
// { dg-options "-O2 -fno-tree-fre" }

struct A {};

void
foo (A *p, bool x)
{
  A a;
  char *e = (char *) (&a + 1);
  if (x)
    __builtin_memmove (p, &a, e - (char *) &a);
}
