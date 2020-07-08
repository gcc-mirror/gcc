// PR c++/95303
// { dg-do compile { target c++20 } }

template<class>
struct A {
    struct B {};
};

template<class T>
  requires __is_same(T, char)
struct A<T> {
    struct B {};
};

template<>
  struct A<bool> {
    struct B {};
  };

template<class T>
concept C = requires (T&& t) { // { dg-message "\\\[with T = A<int>::B\\\]" }
    t.a;
};
static_assert(C<A<int>::B>); // { dg-error "failed" }

template<class T>
concept D = requires (T&& t) { // { dg-message "\\\[with T = A<char>::B\\\]" }
    t.a;
};
static_assert(D<A<char>::B>); // { dg-error "failed" }

template<class T>
concept E = requires (T&& t) { // { dg-message "\\\[with T = A<bool>::B\\\]" }
    t.a;
};
static_assert(E<A<bool>::B>); // { dg-error "failed" }
