template <int I> 
struct A {
};

template <typename T>
struct B {
  typedef typename T::type type;
  static const type j = T::j;

  A<j> b;
};

struct C {
  typedef int type;
  static const int j = 3;
};

int i = B<C>::j;

