// PR c++/56323
// { dg-do compile { target c++11 } }

struct A {
  A(int i);
};

typedef A B;

struct C : B {
  using B::B;
};

struct D : B {
  using B::A;
};

C c(0);
D d(0);

template <class T>
struct E {
  typedef T type;
};

template <class T>
struct F : E<T>::type {
  using E<T>::type::type; // error: E<T>::type is a typedef
};

F<A> f(0);

template <class T>
struct AT
{
  AT(T);
};

template <template <class> class T>
struct G : T<int>
{
  using T<int>::T;
};

G<AT> g(0);
