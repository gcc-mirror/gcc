// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X {
  template<typename T, int N = 1>
  explicit(N) operator T();
};

int
main ()
{
  X x;
  int i = x; // { dg-error "cannot convert" }
  int i2{x};
  double d = x; // { dg-error "cannot convert" }
  double d2{x};
  char c = x; // { dg-error "cannot convert" }
  char c2{x};
  long l = x; // { dg-error "cannot convert" }
  long l2{x};
  int *p = x; // { dg-error "cannot convert" }
  int *p2{x};
}
