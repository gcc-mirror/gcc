// { dg-do assemble  }

template<int T>
struct A {
  char *a;
  A (const char* x)
  {
    a = (char*) x;
  }
};

template<int U, int V, class T>
struct B {
  T a[V-U+1];
  friend A<V-U+1> f (B const &x)
  {
    return A<V-U+1> ((char*) x.a);
  }
};

const int a = 8;

typedef B<1,a,int> C;
struct D {
  C x;
};
