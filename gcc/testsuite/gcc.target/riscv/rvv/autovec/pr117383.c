/* { dg-do run } */
/* { dg-require-effective-target "riscv_v_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=zvl" } */

typedef signed char int8_t;
typedef int8_t vnx64i __attribute__ ((vector_size (64)));

#define MASK_64                                                                \
  1, 2, 3, 5, 7, 9, 10, 11, 12, 14, 15, 17, 19, 21, 22, 23, 26, 28, 30, 31,    \
    37, 38, 41, 46, 47, 53, 54, 55, 60, 61, 62, 63, 76, 77, 78, 79, 80, 81,    \
    82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,    \
    100, 101, 102, 103, 104, 105, 106, 107

void __attribute__ ((noipa))
test_1 (int8_t *x, int8_t *y, int8_t *out)
{
  vnx64i v1 = *(vnx64i *) x;
  vnx64i v2 = *(vnx64i *) y;
  vnx64i v3 = __builtin_shufflevector (v1, v2, MASK_64);
  *(vnx64i *) out = v3;
}

int
main (void)
{
  int8_t x[64];
  int8_t y[64];
  int8_t out[64];

  for (int i = 0; i < 64; i++)
    {
      x[i] = -i;
      y[i] = i;
    }

  test_1 (x, y, out);

  int mask[] = {MASK_64};
#pragma GCC novector
  for (int i = 0; i < 64; i++)
    {
      int idx = mask[i] < 64 ? mask[i] : mask[i] - 64;
      int ref = mask[i] < 64 ? x[idx] : y[idx];
      if (ref != out[i])
        __builtin_abort ();
    }
}
