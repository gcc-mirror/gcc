// DR2351
// { dg-do compile { target c++11 } }

void bar (int);

template <typename T>
auto foo (T t) -> decltype (bar (t), void{})
{
  return bar (t);
}

int
main ()
{
  foo (0);
}
