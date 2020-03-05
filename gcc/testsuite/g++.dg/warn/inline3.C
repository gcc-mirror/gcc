struct S {
  inline virtual void foo () = 0;	// { dg-bogus "used but never defined" }
#if __cplusplus > 201703L
  constexpr virtual void bar () = 0;	// { dg-bogus "used but never defined" "" { target c++2a } }
#else
  inline virtual void bar () = 0;	// { dg-bogus "used but never defined" "" { target c++17_down }  }
#endif
  S () {}
};
struct T : public S {
  inline virtual void foo () {}
#if __cplusplus > 201703L
  constexpr virtual void bar () {}
#else
  inline virtual void bar () {}
#endif
  T () {}
};
T t;
void foo (S *s) { s->foo (); s->bar (); }
