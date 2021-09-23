// PR c++/100054
// { dg-do compile { target c++14 } }

template <class T>
void f() {
  struct A { T m{}; };
  [](auto){ return A{}; };
}

template void f<int>();
