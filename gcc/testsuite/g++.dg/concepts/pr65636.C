// { dg-options "-std=c++17 -fconcepts" }

using TD = int;

template<typename T>
concept bool C() {
  return requires () { typename TD; };
}

static_assert(C<int>(), "");
