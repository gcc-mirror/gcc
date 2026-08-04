// PR c++/125280
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <class T, int N>
struct Array {
  T mElements[N];
};

struct Element {
  static const Array<Element, 9> mData;
};
const Array<Element, 9> Element::mData;

struct CE { decltype(^^::) i; };

struct A {
  static const Array<CE, 10> mData;
};
const Array<CE, 10> A::mData{};

struct B {
  static constexpr Array<CE, 11> mData{};
};

struct C {
  static const Array<CE, 12> mData;
};
