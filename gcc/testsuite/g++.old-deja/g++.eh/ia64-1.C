// Special g++ Options: -O2

#include <exception>

using namespace std;

extern "C" void abort();

int i_0, i_1, i_2, i_3, i_4, i_5, i_6, i_7, i_8, i_9;
int j_0, j_1, j_2, j_3, j_4, j_5, j_6, j_7, j_8, j_9;
int k_0, k_1, k_2, k_3, k_4, k_5, k_6, k_7, k_8, k_9;
int l_0, l_1, l_2, l_3, l_4, l_5, l_6, l_7, l_8, l_9;
#define A(x,y,n) register int *x##n = &y##_##n;
#define B(x,y) \
  A(x,y,0) A(x,y,1) A(x,y,2) A(x,y,3) A(x,y,4) \
  A(x,y,5) A(x,y,6) A(x,y,7) A(x,y,8) A(x,y,9)
#define C(x,n) asm volatile ("" : "=r" (x##n) : "0" (x##n));
#define D(x) \
  C(x,0) C(x,1) C(x,2) C(x,3) C(x,4) \
  C(x,5) C(x,6) C(x,7) C(x,8) C(x,9)
#define E(x,y,n) if (x##n != &y##_##n) abort ();
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
