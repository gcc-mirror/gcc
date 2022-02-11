// PR c++/102933
// { dg-do compile { target c++20 } }

template<class T> struct X { T t; };

template<X> void f();

template<class T>
void g() {
  f<X{T{0}}>();
}

template void g<int>();
