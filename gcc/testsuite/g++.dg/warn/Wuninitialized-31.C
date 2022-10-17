// PR c++/19808
// { dg-do compile }
// { dg-options "-Wuninitialized" }

class AllocatorWithCleanup {
public:
  int *allocate(int);
};
class SecBlock {
  SecBlock() : m_ptr(m_alloc.allocate(0)) {} // { dg-bogus "uninitialized" }
  AllocatorWithCleanup m_alloc;
  int *m_ptr;
};

struct A {
  int *allocate(int);
};

struct B {
  int : 0;
  int *allocate(int);
};

struct C : B {
};

struct D {
  char arr[0];
  int *allocate(int);
};

struct E { };

struct F {
  E arr[10];
  int *allocate(int);
};

struct G {
  E e;
  int *allocate(int);
};

struct H {
  virtual void foo ();
  int *allocate(int);
};

template<typename T>
struct X {
  X() : m_ptr(t.allocate(0)) {} // { dg-bogus "uninitialized" }
  T t;
  int *m_ptr;
};

struct V {
  int a;
  int *allocate(int);
};

struct Z {
  Z() : m_ptr(v.allocate(0)) {} // { dg-warning "uninitialized" }
  V v;
  int *m_ptr;
};

X<A> x1;
X<B> x2;
X<C> x3;
X<D> x4;
X<F> x5;
X<G> x6;
X<H> x7;
