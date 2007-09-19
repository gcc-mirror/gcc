// Valid __thread specifiers.
// { dg-require-effective-target tls_native }

__thread int g1;
extern __thread int g2;
static __thread int g3;

void foo()
{
  extern __thread int l1;
  static __thread int l2;
}

struct A {
  static __thread int i;
};

__thread int A::i = 42;

template <typename T> struct B {
  static __thread T t;
};

template <typename T>
__thread T B<T>::t = 42;

void bar ()
{
  int j = B<int>::t;
  int k = B<const int>::t;
}
