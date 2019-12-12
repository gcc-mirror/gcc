// PR c++/84518
// { dg-do compile { target c++11 } }

template<typename T> void foo()
{
  T x[=];  // { dg-error "expected" }
  [&x]{};
}
