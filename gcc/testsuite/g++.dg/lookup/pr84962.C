// PR c++/84952 ICE with anon-struct having member fns
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-pedantic }

struct X {
  struct 
  {
    template <typename> int a ();
    // { dg-error "public non-static data member" "" { target *-*-* } .-1 }
  };

  int  : a; // { dg-error "non-integral" }
};

