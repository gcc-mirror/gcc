// PR c++/80059
// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

template<typename T> int foo(T b)
{
  return __transaction_atomic noexcept(b) (0); // { dg-error "is not a constant expression" }
}

void bar()
{
  foo(true);
}
