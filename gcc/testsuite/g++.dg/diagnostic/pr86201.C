// PR c++/86201
// { dg-do compile { target c++11 } }

template <class U, class V>
auto fn1 (V&& v) -> decltype(U(v))
{
  return; // { dg-error "return-statement with no value" }
}
void fn2 ()
{
  fn1<bool>(1.0);
}
