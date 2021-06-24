// PR c++/101174
// { dg-do compile { target c++17 } }

struct S { using type = int; };

template<class T = int, class U = S>
struct multiset {
  using type = typename U::type;
  multiset(T);
  multiset(U);
};

template<class T>
multiset(T) -> multiset<T>;

multiset c(42);
