// PR target/84502
// { dg-do run }

template<typename T>
struct A { };
using X = A<int>;

void
foo (X, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8)
{
  if (a1 != 0 || a2 != 1 || a3 != 2 || a4 != 3
      || a5 != 4 || a6 != 5 || a7 != 6 || a8 != 7)
    __builtin_abort ();
}

int
main ()
{
  foo (X{}, 0, 1, 2, 3, 4, 5, 6, 7);
}
