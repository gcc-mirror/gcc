/* PR target/104502 */
/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-O -flive-range-shrinkage -march=barcelona -fstack-protector-all -mavx512f" } */

typedef char __attribute__((__vector_size__ (8))) U;
typedef int __attribute__((__vector_size__ (8))) A;
typedef int __attribute__((__vector_size__ (16))) B;
typedef int __attribute__((__vector_size__ (32))) C;
typedef int __attribute__((__vector_size__ (64))) D;
typedef __float128 __attribute__((__vector_size__ (32))) F;

char s;
U u;
A a;
int i;
C c;
double d;

U
foo (U u0, A a0, B b0, B b1, C c0, C c1, C c2, C c3, A a1, A a2, F f0)
{
  C ca = c |= (short) (float) d;
  C cb = c0 + c1 + c2 + c3 + ca + (C) f0;
  U ua = s << (u & 4);
  B ba = ((union {C a; B b;}) cb).b + b0 + b1;
  U ub = ((union {B a; U b;}) ba).b +
    u0 + u + ua + (U) a + (U) a + (U) a0 + (U) a1 + (U) a2;
  long long u64_r = i + d;
  char u8_r = u64_r;
  return ub + u8_r;
}
