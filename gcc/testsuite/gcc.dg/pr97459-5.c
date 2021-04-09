/* PR rtl-optimization/97459 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-DEXPENSIVE" { target run_expensive_tests } } */

#ifdef __SIZEOF_INT128__
typedef __uint128_t T;
#else
typedef unsigned long long T;
#endif

T __attribute__((noipa)) foo (T x, T n, T *r) { *r = x % n; return x / n; }
#define C(n) T __attribute__((noipa)) foo##n (T x, T *r) { *r = x % (n - 10000); return x / (n - 10000); }

#define C1(n) C(n##1) C(n##3) C(n##5) C(n##7) C(n##9)
#define C2(n) C1(n##0) C1(n##1) C1(n##2) C1(n##3) C1(n##4) \
	      C1(n##5) C1(n##6) C1(n##7) C1(n##8) C1(n##9)
#ifdef EXPENSIVE
#define C3(n) C2(n##0) C2(n##1) C2(n##2) C2(n##3) C2(n##4) \
	      C2(n##5) C2(n##6) C2(n##7) C2(n##8) C2(n##9)
#define C4(n) C3(n##0) C3(n##1) C3(n##2) C3(n##3) C3(n##4) \
	      C3(n##5) C3(n##6) C3(n##7) C3(n##8) C3(n##9)
#else
#define C3(n) C2(n##0) C2(n##4) C2(n##9)
#define C4(n) C3(n##0) C3(n##3) C3(n##7)
#endif
#define TESTS C4(1) C1(10010) C1(10012) C1(16144)

TESTS

struct S { T x; T (*foo) (T, T *); };

#undef C
#define C(n) { n - 10000, foo##n },

struct S tests[] = {
TESTS
  { 0, 0 }
};

int
main ()
{
  int i, j, k;
  for (k = 0; tests[k].x; k++)
    for (i = 0; i < sizeof (T) * __CHAR_BIT__; i++)
      for (j = -5; j <= 5; j++)
	{
	  T x = ((T) 1 << i) + j;
	  T r1, r2;
	  if (foo (x, tests[k].x, &r1) != tests[k].foo (x, &r2)
	      || r1 != r2)
	    __builtin_abort ();
	}
  return 0;
}
