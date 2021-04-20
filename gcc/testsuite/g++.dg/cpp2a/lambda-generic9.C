// PR c++/95434
// { dg-do compile { target c++20 } }

template <class>
void f() {
  [] <template <class> class U> () { U{0}; };
}

template void f<int>();
