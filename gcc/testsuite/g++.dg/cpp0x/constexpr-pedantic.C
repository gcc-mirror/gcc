// The FCD doesn't allow typedefs and static_assert in constexpr functions,
// but it should.
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

template <class T>
constexpr T f(T t)
{
  typedef T T2;			     // { dg-warning "constexpr" "" { xfail *-*-* } }
  static_assert (T2(0) == T(0), ""); // { dg-warning "constexpr" "" { xfail *-*-* } }
  return t;
}

int main()
{
  constexpr int i = f(42);
}
