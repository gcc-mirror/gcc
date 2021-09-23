// PR c++/101247
// { dg-do compile { target concepts } }
// A variant of concepts-memtmpl5.C that uses a partial specialization
// of A instead of the primary template.

template<class, class> struct A;

template<class T, class U> requires true struct A<T, U> {
  template<class V> static constexpr bool d = true;
  static void g() requires d<U>;
};

int main() {
  A<int, char>::g();
}
