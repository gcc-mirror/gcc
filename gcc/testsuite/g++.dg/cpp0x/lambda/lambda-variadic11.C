// PR c++/97358
// { dg-do compile { target c++11 } }

template <typename... T> void foo (T... x) {}

template <typename... T> void bar (T... x)
{
  foo ([x...] { return x; }...); // { dg-error "not expanded|no parameter packs" }
#if __cpp_init_captures >= 201803L
  foo ([...y = x] { return y; }...); // { dg-error "not expanded|no parameter packs" "" { target c++20 } }
#endif
}

void
test ()
{
  bar ();
  bar (1);
  bar (2.0, 3LL, 4);
}
