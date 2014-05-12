// PR c++/60054
// { dg-do compile { target c++1y } }

template<typename T> T fooA(T);
template<typename T> decltype(T{}) fooB(T);

void bar()
{
  fooA((auto*)0);  // { dg-error "invalid use" }
  fooB((auto*)0);  // { dg-error "invalid use" }
}
