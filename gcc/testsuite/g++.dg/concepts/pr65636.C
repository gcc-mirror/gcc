// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

using TD = int;

template<typename T>
concept bool C() {
  return requires () { typename TD; };
}

static_assert(C<int>(), "");
