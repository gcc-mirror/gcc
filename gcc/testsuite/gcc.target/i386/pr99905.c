/* PR rtl-optimization/99905 */
/* { dg-do run { target int128 } } */
/* { dg-options "-Os -mno-mmx -mno-sse" } */

typedef unsigned char U;
typedef unsigned char __attribute__((__vector_size__ (8))) A;
typedef unsigned char __attribute__((__vector_size__ (16))) B;
typedef unsigned char __attribute__((__vector_size__ (32))) C;
typedef unsigned int __attribute__((__vector_size__ (8))) D;
typedef unsigned long long __attribute__((__vector_size__ (8))) E;
typedef unsigned __int128 I;
typedef unsigned long long L;

D gv;
I gi;

L __attribute__((__noipa__))
foo (int ua, int ub, int uc, int ud, E ue, I i)
{
  D d = (U) __builtin_bswap16 (i >> 63) + gv;
  B y = ((union { C a; B b[2];}) (C){ }).b[0] + (B) gi;
  A z = ((union { B a; A b[2];}) y).b[0] + (A) d;
  return (L)z;
}

int
main ()
{
  L x = foo (0, 0, 0, 0, (E) { }, (I) 0x100 << 63);
  if (x != 0x100000001)
    __builtin_abort ();
  return 0;
}
