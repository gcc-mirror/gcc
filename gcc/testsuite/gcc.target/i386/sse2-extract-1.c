/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2_runtime } */

extern void abort (void);
typedef unsigned long long uint64_t;

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define FN(elcount, type, idx) \
__attribute__((noinline, noclone)) \
type f##type##elcount##_##idx (vector (elcount, type) x) { return x[idx] + 1; }
#define T2(elcount, type) \
  H (elcount, type) \
  F (elcount, type, 0) \
  F (elcount, type, 1)
#define T4(elcount, type) \
  T2 (elcount, type) \
  F (elcount, type, 2) \
  F (elcount, type, 3)
#define T8(elcount, type) \
  T4 (elcount, type) \
  F (elcount, type, 4) \
  F (elcount, type, 5) \
  F (elcount, type, 6) \
  F (elcount, type, 7)
#define T16(elcount, type) \
  T8 (elcount, type) \
  F (elcount, type, 8) \
  F (elcount, type, 9) \
  F (elcount, type, 10) \
  F (elcount, type, 11) \
  F (elcount, type, 12) \
  F (elcount, type, 13) \
  F (elcount, type, 14) \
  F (elcount, type, 15)
#define T32(elcount, type) \
  T16 (elcount, type) \
  F (elcount, type, 16) \
  F (elcount, type, 17) \
  F (elcount, type, 18) \
  F (elcount, type, 19) \
  F (elcount, type, 20) \
  F (elcount, type, 21) \
  F (elcount, type, 22) \
  F (elcount, type, 23) \
  F (elcount, type, 24) \
  F (elcount, type, 25) \
  F (elcount, type, 26) \
  F (elcount, type, 27) \
  F (elcount, type, 28) \
  F (elcount, type, 29) \
  F (elcount, type, 30) \
  F (elcount, type, 31)
#define TESTS_SSE2 \
T2 (2, double) E \
T2 (2, uint64_t) E \
T4 (4, float) E \
T4 (4, int) E \
T8 (8, short) E \
T16 (16, char) E
#define TESTS_AVX \
T4 (4, double) E \
T4 (4, uint64_t) E \
T8 (8, float) E \
T8 (8, int) E \
T16 (16, short) E \
T32 (32, char) E
#ifdef __AVX__
#define TESTS TESTS_SSE2 TESTS_AVX
#else
#define TESTS TESTS_SSE2
#endif

#define F FN
#define H(elcount, type)
#define E
TESTS

int
main ()
{
#undef F
#undef H
#undef E
#define H(elcount, type) \
  vector (elcount, type) v##type##elcount = {
#define E };
#define F(elcount, type, idx) idx + 1,
  TESTS
#undef F
#undef H
#undef E
#define H(elcount, type)
#define E
#define F(elcount, type, idx) \
  if (f##type##elcount##_##idx (v##type##elcount) != idx + 2) \
    abort ();
  TESTS
  return 0;
}
