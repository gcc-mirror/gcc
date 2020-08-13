// PR c++/93529
// P1009: Array size deduction in new-expressions
// { dg-do compile { target c++11 } }

template<typename... T>
int *fn(T... t)
{
  return new int[]{t...};
}

int
main ()
{
  int *p0 = fn ();
  int *p1 = fn (1);
  int *p3 = fn (1, 2, 3);
}
