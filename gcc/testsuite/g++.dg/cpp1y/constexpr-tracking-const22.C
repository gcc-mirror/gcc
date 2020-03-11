// PR c++/94074 - wrong modifying const object error for COMPONENT_REF.
// { dg-do compile { target c++14 } }

struct X {
  int i;
};

template <typename T>
struct S {
  const X x;
  constexpr S(int) : x{}
  {
    const_cast<X&>(x).i = 19; // { dg-error "modifying a const object" }
  }
};

constexpr S<int> p = { 10 }; // { dg-message "originally declared" }
