// PR c++/117792
// { dg-do compile { target c++17 } }

template<const int& N, class T>
void f(T) { }

template<int N, class T>
void f(...) = delete;

template<const int& N> int v;

template<const int& N> struct A { };

template<class T>
void g() {
  static constexpr int local_static = 0;
  auto x = v<local_static>; // OK
  A<local_static> y; // OK
  f<local_static>(0); // ICE
}

template void g<int>();
