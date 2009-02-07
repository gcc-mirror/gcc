// { dg-options "-std=c++0x" }

template<typename _Tp>
  _Tp&& forward(_Tp&& __t) { return __t; }

void f(...);

template<typename... Args>
void g(Args&&... args)
{
  f(forward<Args...>(args...)); // { dg-error "no matching" }
}

void h()
{
  g();
}
