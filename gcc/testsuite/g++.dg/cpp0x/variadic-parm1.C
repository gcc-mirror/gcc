// PR c++/93140
// { dg-do compile { target c++11 } }

int
bar ()
{
  return 42;
}

template <typename... R>
void foo (R... r, decltype (bar (r...)) x = 0) {}

int
main ()
{
  foo (3);
}
