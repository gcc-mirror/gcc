// PR c++/83556
// { dg-do run { target c++11 } }

int
foo ()
{
  return 1;
}

struct A
{
  int a = foo ();
  int b = 1;
  int c = a ? 1 * b : 2 * b;
};

struct B
{
  A d {};
};

int
main ()
{
  B e {};
  if (e.d.c != 1)
    __builtin_abort ();
}
