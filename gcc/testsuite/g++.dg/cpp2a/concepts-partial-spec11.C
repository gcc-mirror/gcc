// PR c++/103678
// { dg-do compile { target c++20 } }

template<class>
struct A {
 template<class...>
 struct B;
};

template<class A_t>
template<class B_t>
struct A<A_t>::B<B_t> {};

template<class A_t>
template<class B_t>
requires requires {
 typename B_t;
}
struct A<A_t>::B<B_t> {};
