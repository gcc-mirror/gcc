// PR c++/101247
// { dg-do compile { target concepts } }

template<class T, class U> struct A {
  template<class> static constexpr bool d = true;
  static void g() requires d<U>;
};

int main() {
  A<int, char>::g();
}
