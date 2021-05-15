/* PR rtl-optimization/100342 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -fno-dse -fno-forward-propagate -Wno-psabi -mno-sse2" } */

#define SHL(x, y) ((x) << ((y) & (sizeof(x) * 8 - 1)))
#define SHR(x, y) ((x) >> ((y) & (sizeof(x) * 8 - 1)))
#define ROR(x, y) (SHR(x, y)) | (SHL(x, (sizeof(x) * 8 - (y))))
#define SHLV(x, y) ((x) << ((y) & (sizeof((x)[0]) * 8 - 1)))
#define SHLSV(x, y) ((x) << ((y) & (sizeof((y)[0]) * 8 - 1)))
typedef unsigned char A;
typedef unsigned char __attribute__((__vector_size__ (8))) B;
typedef unsigned char __attribute__((__vector_size__ (16))) C;
typedef unsigned char __attribute__((__vector_size__ (32))) D;
typedef unsigned char __attribute__((__vector_size__ (64))) E;
typedef unsigned short F;
typedef unsigned short __attribute__((__vector_size__ (16))) G;
typedef unsigned int H;
typedef unsigned int __attribute__((__vector_size__ (32))) I;
typedef unsigned long long J;
typedef unsigned long long __attribute__((__vector_size__ (8))) K;
typedef unsigned long long __attribute__((__vector_size__ (32))) L;
typedef unsigned long long __attribute__((__vector_size__ (64))) M;
typedef unsigned __int128 N;
typedef unsigned __int128 __attribute__((__vector_size__ (16))) O;
typedef unsigned __int128 __attribute__((__vector_size__ (32))) P;
typedef unsigned __int128 __attribute__((__vector_size__ (64))) Q;
B v1;
D v2;
L v3;
K v4;
I v5;
O v6;

B
foo (A a, C b, E c, F d, G e, H f, J g, M h, N i, P j, Q k)
{
  b &= (A) f;
  k += a;
  G l = e;
  D m = v2 >= (A) (J) v1;
  J r = a + g;
  L n = v3 <= f;
  k -= i / f;
  l -= (A) g;
  c |= (A) d;
  b -= (A) i;
  J o = ROR (__builtin_clz (r), a);
  K p = v4 | f, q = v4 <= f;
  P s = SHLV (SHLSV (__builtin_bswap64 (i), (P) (0 < j)) <= 0, j);
  n += a <= r;
  M t = (M) (a / SHLV (c, 0)) != __builtin_bswap64 (i);
  I u = f - v5;
  E v = (E) h + (E) t + (E) k;
  D w = (union { D b[2]; }) { }.b[0] + ((union { E b; }) v).b[1] + m + (D) u + (D) n + (D) s;
  C x = ((union { D b; }) w).b[1] + b + (C) l + (C) v6;
  B y = ((union { C a; B b; }) x).b + ((union { C a; B b[2]; }) x).b[1] + (B) p + (B) q;
  J z = i + o;
  F z2 = z;
  A z3 = z2;
  return y + z3;
}

int
main ()
{
  B x = foo (0, (C) { }, (E) { }, 10, (G) { }, 4, 2, (M) { }, 123842323652213865LL, (P) { 1 }, (Q) { });
  if ((J) x != 0x2e2c2e2c2e2c2e30ULL)
    __builtin_abort();
  return 0;
}
