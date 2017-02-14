/* { dg-do run } */
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-pre-stats" } */

struct A
{
  int i;
};

struct B
{
  struct A a[2];
};

int i = 1;
struct B b = { 0, 3 };

void
test ()
{
  if (b.a[0].i != i)
    {
      int t = b.a[0].i;
      b.a[0] = b.a[1];
      b.a[1].i = t;
    }

  if (b.a[1].i == i)
    __builtin_abort ();

  if (b.a[0].i == 0)
    __builtin_abort ();
}

int
main ()
{
  test ();
  return 0;
}

/* { dg-final { scan-tree-dump "Eliminated: 1" "pre" } } */
