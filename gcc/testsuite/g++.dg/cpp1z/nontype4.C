// PR c++/98930
// { dg-do compile { target c++17 } }

template<int*>
struct A { A() { } };

template<class T>
void impl() {
  static int i;
  static A<&i> a;
}

template void impl<int>();
template void impl<char>();
