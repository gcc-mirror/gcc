/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3 -mrvv-max-lmul=m8" } */

typedef short v32hi __attribute__ ((vector_size (64)));
typedef long long v16di __attribute__ ((vector_size (128)));

#define E16 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30
#define O16 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31
#define E32 E16, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62
#define O32 O16, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63

#define LOOP(NAME, TYPE, A, B, MASK)                                           \
  __attribute__ ((noipa)) TYPE NAME (TYPE x, TYPE y, int n)                   \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      x = __builtin_shufflevector (A, B, MASK);                                \
    return x;                                                                  \
  }

LOOP (hi_e_xy, v32hi, x, y, E32)
LOOP (hi_e_yx, v32hi, y, x, E32)
LOOP (hi_o_xy, v32hi, x, y, O32)
LOOP (hi_o_yx, v32hi, y, x, O32)
LOOP (di_e_xy, v16di, x, y, E16)
LOOP (di_e_yx, v16di, y, x, E16)
LOOP (di_o_xy, v16di, x, y, O16)
LOOP (di_o_yx, v16di, y, x, O16)

#define CHECK(FN, TYPE, ETYPE, N, ODD, SWAP)                                   \
  {                                                                            \
    TYPE a, b;                                                                 \
    ETYPE x[N], y[N], c[2 * N];                                                \
    for (int k = 0; k < N; k++)                                                \
      {                                                                        \
	a[k] = x[k] = k + 1;                                                   \
	b[k] = y[k] = 100 + k;                                                 \
      }                                                                        \
    for (int i = 0; i < 3; i++)                                                \
      {                                                                        \
	for (int k = 0; k < N; k++)                                            \
	  {                                                                    \
	    c[k] = SWAP ? y[k] : x[k];                                         \
	    c[N + k] = SWAP ? x[k] : y[k];                                     \
	  }                                                                    \
	for (int k = 0; k < N; k++)                                            \
	  x[k] = c[2 * k + ODD];                                               \
      }                                                                        \
    TYPE r = FN (a, b, 3);                                                     \
    for (int k = 0; k < N; k++)                                                \
      if (r[k] != x[k])                                                        \
	__builtin_abort ();                                                    \
  }

int
main ()
{
  CHECK (hi_e_xy, v32hi, short, 32, 0, 0)
  CHECK (hi_e_yx, v32hi, short, 32, 0, 1)
  CHECK (hi_o_xy, v32hi, short, 32, 1, 0)
  CHECK (hi_o_yx, v32hi, short, 32, 1, 1)
  CHECK (di_e_xy, v16di, long long, 16, 0, 0)
  CHECK (di_e_yx, v16di, long long, 16, 0, 1)
  CHECK (di_o_xy, v16di, long long, 16, 1, 0)
  CHECK (di_o_yx, v16di, long long, 16, 1, 1)
  return 0;
}
