// { dg-options "-std=c++17 -fconcepts" }

struct fool {
  constexpr fool operator&&(fool) const { return {}; }
  constexpr fool operator||(fool) const { return {}; }
};

template<typename T> constexpr fool p1() { return {}; }
template<typename T> constexpr fool p2() { return {}; }

template<typename T>
  concept bool C() { return p1<T>() && p2<T>(); }

template<C T> void f(T x) { }

int main() {
  f(0); // { dg-error "cannot call|uses overloaded operator" }
}
