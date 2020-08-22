// PR c++/94937 - ICE with -Wall and constexpr if.
// { dg-do compile { target c++17 } }
// { dg-options "-Wall" }

struct B {
  static constexpr bool foo() { return false; }
};

template<typename T>
struct C {
  static void bar ()
  {
    if constexpr (B::foo()) ;
  }
};
