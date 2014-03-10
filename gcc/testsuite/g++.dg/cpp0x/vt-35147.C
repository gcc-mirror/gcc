// { dg-do compile { target c++11 } }

template<typename _Tp>
  _Tp&& forward(_Tp&& __t) { return __t; } // { dg-message "note" }

void f(...);

template<typename... Args>
void g(Args&&... args)
{
  f(forward<Args...>(args...)); // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 11 }
}

void h()
{
  g();
}
