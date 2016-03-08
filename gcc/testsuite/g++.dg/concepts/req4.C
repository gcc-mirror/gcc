// { dg-options "-std=c++1z -fconcepts" }

struct fool {
  constexpr fool operator&&(fool) const { return {}; }
  constexpr fool operator||(fool) const { return {}; }
};

template<typename T> constexpr fool p1() { return {}; }
template<typename T> constexpr fool p2() { return {}; }

template<typename T>
  concept bool C() { return p1<T>() && p2<T>(); } // { dg-error "does not have type" }

template<C T> void f(T x) { }

int main() {
  f(0); // { dg-error "cannot call" }
}
