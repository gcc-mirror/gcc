// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<decltype (^^::) R>
void
bar ()
{
  constexpr auto t = [: R :]; // { dg-error "use of local variable" }
}

void
g (int p)
{
  int x = 42;
  bar<^^p> ();
  constexpr auto r = ^^x;
  struct S {
    void foo () {
      [: ^^x :];    // { dg-error "use of local variable" }
      [: r :];	    // { dg-error "use of local variable" }
      decltype ([: ^^x :]) a{};
      decltype ([: r :]) b{};
      constexpr auto rx = ^^x;
    }
  };
}
