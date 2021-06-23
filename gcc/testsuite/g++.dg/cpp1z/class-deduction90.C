// PR c++/86439
// { dg-do compile { target c++17 } }

struct less { };
struct allocator { };

template<class T, class U = less, class V = allocator>
struct A {
  A(T, U);
  A(T, V);
};

template<class T, class U = less>
A(T, U) -> A<T>;

A a(0, {}); // { dg-error "ambiguous" }
