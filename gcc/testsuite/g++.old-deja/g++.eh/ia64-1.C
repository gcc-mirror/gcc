// Special g++ Options: -O2

#include <exception>

using namespace std;

extern "C" void abort();

int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9;
int j0, j1, j2, j3, j4, j5, j6, j7, j8, j9;
int k0, k1, k2, k3, k4, k5, k6, k7, k8, k9;
int l0, l1, l2, l3, l4, l5, l6, l7, l8, l9;
#define A(x,y,n) register int *x##n = &y##n;
#define B(x,y) \
  A(x,y,0) A(x,y,1) A(x,y,2) A(x,y,3) A(x,y,4) \
  A(x,y,5) A(x,y,6) A(x,y,7) A(x,y,8) A(x,y,9)
#define C(x,n) asm volatile ("" : "=r" (x##n) : "0" (x##n));
#define D(x) \
  C(x,0) C(x,1) C(x,2) C(x,3) C(x,4) \
  C(x,5) C(x,6) C(x,7) C(x,8) C(x,9)
#define E(x,y,n) if (x##n != &y##n) abort ();
#define F(x,y) \
  E(x,y,0) E(x,y,1) E(x,y,2) E(x,y,3) E(x,y,4) \
  E(x,y,5) E(x,y,6) E(x,y,7) E(x,y,8) E(x,y,9)

void bar(long a0, long a1, long a2, long a3, long a4)
{
}

void foo(long a0, long a1, long a2, long a3, long a4)
{
  A(p,l,0) A(p,l,1) A(p,l,2)
  C(p,0) C(p,1) C(p,2)
  bar (0, 1, 2, 3, 4);
  if (a0 == 0)
    throw exception();
  C(p,0) C(p,1) C(p,2)
  E(p,l,0) E(p,l,1) E(p,l,2)
}

void test(void)
{
  A(p,l,0) A(p,l,1) A(p,l,2) A(p,l,3) A(p,l,4) A(p,l,5) A(p,l,6)
  C(p,0) C(p,1) C(p,2) C(p,3) C(p,4) C(p,5) C(p,6)
  try {
    foo(0, 1, 2, 3, 4);
  } catch (exception) {}
  C(p,0) C(p,1) C(p,2) C(p,3) C(p,4) C(p,5) C(p,6)
  E(p,l,0) E(p,l,1) E(p,l,2) E(p,l,3) E(p,l,4) E(p,l,5) E(p,l,6)
}

int main()
{
  B(x,i)
  B(y,j)
  B(z,k)
  A(p,l,0) A(p,l,1) A(p,l,2) A(p,l,3)
  D(x)
  D(y)
  D(z)
  C(p,0) C(p,1) C(p,2) C(p,3)
  test();
  D(x)
  D(y)
  D(z)
  C(p,0) C(p,1) C(p,2) C(p,3)
  F(x,i)
  F(y,j)
  F(z,k)
  E(p,l,0) E(p,l,1) E(p,l,2) E(p,l,3)
  return 0;
}
