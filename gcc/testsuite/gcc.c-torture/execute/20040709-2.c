/* Test arithmetics on bitfields.  */

extern void abort (void);
extern void exit (int);

unsigned int
myrnd (void)
{
  static unsigned int s = 1388815473;
  s *= 1103515245;
  s += 12345;
  return (s / 65536) % 2048;
}

#define T(S)					\
struct S s##S;					\
struct S retme##S (struct S x)			\
{						\
  return x;					\
}						\
						\
unsigned int fn1##S (unsigned int x)		\
{						\
  struct S y = s##S;				\
  y.k += x;					\
  y = retme##S (y);				\
  return y.k;					\
}						\
						\
unsigned int fn2##S (unsigned int x)		\
{						\
  struct S y = s##S;				\
  y.k += x;					\
  y.k %= 15;					\
  return y.k;					\
}						\
						\
unsigned int retit##S (void)			\
{						\
  return s##S.k;				\
}						\
						\
unsigned int fn3##S (unsigned int x)		\
{						\
  s##S.k += x;					\
  return retit##S ();				\
}						\
						\
void test##S (void)				\
{						\
  int i;					\
  unsigned int mask, v, a, r;			\
  struct S x;					\
  char *p = (char *) &s##S;			\
  for (i = 0; i < sizeof (s##S); ++i)		\
    *p++ = myrnd ();				\
  if (__builtin_classify_type (s##S.l) == 8)	\
    s##S.l = 5.25;				\
  s##S.k = -1;					\
  mask = s##S.k;				\
  v = myrnd ();					\
  a = myrnd ();					\
  s##S.k = v;					\
  x = s##S;					\
  r = fn1##S (a);				\
  if (x.i != s##S.i || x.j != s##S.j		\
      || x.k != s##S.k || x.l != s##S.l		\
      || ((v + a) & mask) != r)			\
    abort ();					\
  v = myrnd ();					\
  a = myrnd ();					\
  s##S.k = v;					\
  x = s##S;					\
  r = fn2##S (a);				\
  if (x.i != s##S.i || x.j != s##S.j		\
      || x.k != s##S.k || x.l != s##S.l		\
      || ((((v + a) & mask) % 15) & mask) != r)	\
    abort ();					\
  v = myrnd ();					\
  a = myrnd ();					\
  s##S.k = v;					\
  x = s##S;					\
  r = fn3##S (a);				\
  if (x.i != s##S.i || x.j != s##S.j		\
      || s##S.k != r || x.l != s##S.l		\
      || ((v + a) & mask) != r)			\
    abort ();					\
}

#define pck __attribute__((packed))
struct pck A { unsigned short i : 1, l : 1, j : 3, k : 11; }; T(A)
struct pck B { unsigned short i : 4, j : 1, k : 11; unsigned int l; }; T(B)
struct pck C { unsigned int l; unsigned short i : 4, j : 1, k : 11; }; T(C)
struct pck D { unsigned long long l : 6, i : 6, j : 23, k : 29; }; T(D)
struct pck E { unsigned long long l, i : 12, j : 23, k : 29; }; T(E)
struct pck F { unsigned long long i : 12, j : 23, k : 29, l; }; T(F)
struct pck G { unsigned short i : 1, j : 1, k : 6; unsigned long long l; }; T(G)
struct pck H { unsigned short i : 6, j : 2, k : 8; unsigned long long l; }; T(H)
struct pck I { unsigned short i : 1, j : 6, k : 1; unsigned long long l; }; T(I)
struct pck J { unsigned short i : 1, j : 8, k : 7; unsigned short l; }; T(J)
struct pck K { unsigned int k : 6, l : 1, j : 10, i : 15; }; T(K)
struct pck L { unsigned int k : 6, j : 11, i : 15; unsigned int l; }; T(L)
struct pck M { unsigned int l; unsigned short k : 6, j : 11, i : 15; }; T(M)
struct pck N { unsigned long long l : 6, k : 6, j : 23, i : 29; }; T(N)
struct pck O { unsigned long long l, k : 12, j : 23, i : 29; }; T(O)
struct pck P { unsigned long long k : 12, j : 23, i : 29, l; }; T(P)
struct pck Q { unsigned short k : 12, j : 1, i : 3; unsigned long long l; }; T(Q)
struct pck R { unsigned short k : 2, j : 11, i : 3; unsigned long long l; }; T(R)
struct pck S { unsigned short k : 1, j : 6, i : 9; unsigned long long l; }; T(S)
struct pck T { unsigned short k : 1, j : 8, i : 7; unsigned short l; }; T(T)
struct pck U { unsigned short j : 6, k : 1, i : 9; unsigned long long l; }; T(U)
struct pck V { unsigned short j : 8, k : 1, i : 7; unsigned short l; }; T(V)
struct pck W { long double l; unsigned int k : 12, j : 13, i : 7; }; T(W)
struct pck X { unsigned int k : 12, j : 13, i : 7; long double l; }; T(X)
struct pck Y { unsigned int k : 12, j : 11, i : 9; long double l; }; T(Y)
struct pck Z { long double l; unsigned int j : 13, i : 7, k : 12; }; T(Z)

int
main (void)
{
  testA ();
  testB ();
  testC ();
  testD ();
  testE ();
  testF ();
  testG ();
  testH ();
  testI ();
  testJ ();
  testK ();
  testL ();
  testM ();
  testN ();
  testO ();
  testP ();
  testQ ();
  testR ();
  testS ();
  testT ();
  testU ();
  testV ();
  testW ();
  testX ();
  testY ();
  testZ ();
  exit (0);
}
