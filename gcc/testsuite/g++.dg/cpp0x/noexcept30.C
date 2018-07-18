// PR c++/69300
// { dg-do compile { target c++11 } }

template<typename A>
struct F {
  template<typename B>
  void f() noexcept(&F::template f<B>) {} // { dg-error "exception specification" }
};

int main () {
  F<void>().f<int>();
}
