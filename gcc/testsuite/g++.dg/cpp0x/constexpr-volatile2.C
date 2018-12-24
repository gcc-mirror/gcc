// PR c++/86608
// { dg-do compile { target c++11 } }

template<typename T, T v> struct X {};

int
main ()
{
  static constexpr volatile int a = 3;
  constexpr volatile int b = 2;
  return (sizeof(X<decltype(a), a>) // { dg-error "lvalue-to-rvalue conversion of a volatile lvalue" }
	  + sizeof(X<decltype(b), b>)); // { dg-error "lvalue-to-rvalue conversion of a volatile lvalue" }
}
