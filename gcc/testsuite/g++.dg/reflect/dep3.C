// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  template<typename T>
  struct NS { };

  template<typename T>
  void foo ();

  template<typename T>
  S &operator|(const T&);

  template<typename T>
  static int V;
};

template<typename T>
void
g ()
{
  // Looks like r1 and r4 should work.
  //constexpr auto r1 = ^^T::template NS;
  constexpr auto r2 = ^^T::template foo;
  constexpr auto r3 = ^^T::template operator|;
  //constexpr auto r4 = ^^T::template V;
}

void
f ()
{
  g<S>();
}
