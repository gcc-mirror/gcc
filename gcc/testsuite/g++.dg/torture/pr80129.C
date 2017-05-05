// PR c++/80129
// { dg-do run }
// { dg-options "-std=c++11" }

struct A { bool a; int b; };

int
main ()
{
  bool c = false;
  const A x = c ? A {true, 1} : A {false, 0};
  if (x.a)
    __builtin_abort ();
}
