// { dg-options "-std=c++0x" }

auto f() -> int
{
  return 0;
}

template<class T, class U>
auto add(T t, U u) -> decltype (t+u); // { dg-bogus "not declared" "" { xfail *-*-* } }
