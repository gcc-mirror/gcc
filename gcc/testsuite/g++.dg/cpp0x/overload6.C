// PR c++/107461
// { dg-do compile { target c++11 } }

template<class T> T f();

template<class> struct A { };

template<class T> struct B {
  template<class U, class = A<decltype(f<T>()(U()))>>
  static void g(U);
};

int main() {
  B<int> b;
  B<void(*)(int)>::g(0); // { dg-bogus "no match" }
}
