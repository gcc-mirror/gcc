// { dg-do compile }

// Origin: ajl13@bellatlantic.net

// PR c++/5421: ICE for specialization of member function template
// as friend.

struct B {
  template <class T> void b();
};

template <class T> class A {
  friend void B::b<T>();
};

static A<int> a;
