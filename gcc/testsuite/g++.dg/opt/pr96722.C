// PR tree-optimization/96722
// { dg-do run }
// { dg-options "-O2" }

struct S { int s; ~S () {} };

void
foo (S *a)
{
  if (a)
    return;
  a->~S ();
}

int
main ()
{
  S s;
  foo (&s);
}
