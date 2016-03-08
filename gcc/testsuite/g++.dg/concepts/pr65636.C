// { dg-options "-std=c++1z -fconcepts" }

using TD = int;

template<typename T>
concept bool C() {
  return requires () { typename TD; };
}

static_assert(C<int>(), "");
