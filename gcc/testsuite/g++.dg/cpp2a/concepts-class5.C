// PR c++/96830
// { dg-do compile { target c++20 } }

template<class T> concept C = true;

template<C T> requires true
struct A {
  void f();
  template<class U> void g();

  struct B;
  template<class U> struct C;

  static int D;
  template<class U> static int E;
};

template<C T>
void A<T>::f() { }  // { dg-error "different constraints" }

template<class T> requires true
template<class U>
void A<T>::g() { }  // { dg-error "different constraints" }

template<C T> requires (!!true)
struct A<T>::B { }; // { dg-error "different constraints" }

template<class T> requires true
template<class U>
struct A<T>::C { }; // { dg-error "different constraints" }

template<C T>
int A<T>::D = 0;    // { dg-error "different constraints" }

template<class T> requires true
template<class U>
int A<T>::E = 0;    // { dg-error "different constraints" }
