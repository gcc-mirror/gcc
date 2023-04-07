// PR c++/96830
// { dg-do compile { target c++20 } }

template<class T> requires true
struct A {
  template<class U> friend struct A;                   // { dg-error "different constraints" }
  template<class U> requires (!!true) friend struct A; // { dg-error "different constraints" }
};

template<class T>
struct B {
  template<class U> requires true friend struct A;
  template<class U> requires true friend struct B; // { dg-error "different constraints" }
};

template<class T> concept C = true;

template<C T>
struct D {
  template<class U> friend struct D; // { dg-error "different constraints" }
  template<C U> friend struct D;
};

template struct A<int>;
template struct B<int>;
template struct D<int>;
