// PR c++/99844
// { dg-do compile { target c++11 } }

template <bool... B>
struct S {
 void fn() noexcept(B); // { dg-error "parameter packs not expanded" }
};

void fn ()
{
  S<true> s;
  s.fn();
}
