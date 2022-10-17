// PR c++/60054
// { dg-do compile { target c++14 } }

template<typename T> T fooA(T);
template<typename T> decltype(T{}) fooB(T);

void bar()
{
  fooA((auto*)0);  // { dg-error "expected" }
  fooB((auto*)0);  // { dg-error "expected" }
}
