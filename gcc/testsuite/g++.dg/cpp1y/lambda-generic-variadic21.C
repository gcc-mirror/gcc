// PR c++/91104
// { dg-do run { target c++14 } }

void
test (void (*f)(int, int, int))
{
  f(1, 2, 3);
}

void
check (int a, int b, int c)
{
  if (a != 1 || b != 2 || c != 3)
    __builtin_abort ();
}

int
main ()
{
  test ([](auto... args) {
    check (args...);
  });
  test ([](int a, int b, int c) {
    check (a, b, c);
  });
}
