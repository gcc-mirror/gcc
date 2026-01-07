// PR c++/122922
// { dg-additional-options "-fmodules" }
// { dg-module-cmi VF }

export module VF;

template <int N> struct TTensor {
  friend void foo(TTensor) {}
  TTensor<N - 1> TensorArr[1];
};

template <> struct TTensor<0> {
  friend void foo(TTensor) {}
};

template <typename T = void> void TCampo() {
  foo(TTensor<1>());
}
