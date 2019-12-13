// Test for const_cast to reference (5.2.11/4).
// { dg-do compile { target c++11 } }

template <class T> T&& xval();
template <class T> T& lval();
template <class T> T prval();

struct A { };

int main()
{
  const_cast<int&>(lval<int>());
  const_cast<int&>(xval<int>());   // { dg-error "3:invalid .const_cast. of an rvalue" }
  const_cast<int&>(prval<int>());  // { dg-error "3:invalid .const_cast. of an rvalue" }
  const_cast<int&&>(lval<int>());
  const_cast<int&&>(xval<int>());
  const_cast<int&&>(prval<int>()); // { dg-error "3:invalid .const_cast. of an rvalue" }
  const_cast<A&&>(lval<A>());
  const_cast<A&&>(xval<A>());
  const_cast<A&&>(prval<A>());
}
