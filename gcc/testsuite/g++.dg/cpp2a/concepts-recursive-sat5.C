// PR c++/100288
// { dg-do compile { target c++20 } }

struct A { };

template<typename T> concept pipeable = requires(A a, T t) { a | t; }; // { dg-error "depends on itself" }

template<pipeable T> void operator|(A, T);

void f(A tab) {
  tab | 1; // { dg-error "no match" }
  tab | 1; // { dg-error "no match" }
}
