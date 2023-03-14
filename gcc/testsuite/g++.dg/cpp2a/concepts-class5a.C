// PR c++/96830
// A valid version of concepts-class5.C.
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

template<C T> requires true
void A<T>::f() { }

template<C T> requires true
template<class U>
void A<T>::g() { }

template<C T> requires true
struct A<T>::B { };

template<C T> requires true
template<class U>
struct A<T>::C { };

template<C T> requires true
int A<T>::D = 0;

template<C T> requires true
template<class U>
int A<T>::E = 0;
