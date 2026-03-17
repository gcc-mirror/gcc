// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template <typename T>
using A = decltype (T.[:std::meta::nonstatic_data_members_of (^^T, std::meta::access_context::unchecked ())[0]:]);  // { dg-error "expected" }

struct C
{
  int c;
  long d;
};

int
main ()
{
  [] <typename T = C> requires (requires { A<T> {}; })	// { dg-error "expected|not declared" }
    {} ();  // { dg-error "no match" }
}
