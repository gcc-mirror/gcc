/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-require-effective-target rvv_zvl128b_ok } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl -mrvv-max-lmul=m4" } */

typedef unsigned char uint8_t;

void
f (uint8_t *restrict a, uint8_t *restrict c, uint8_t *restrict d, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 8] = c[i * 8] + d[i * 8];
      a[i * 8 + 1] = c[i * 8] + d[i * 8 + 1];
      a[i * 8 + 2] = c[i * 8 + 2] + d[i * 8 + 2];
      a[i * 8 + 3] = c[i * 8 + 2] + d[i * 8 + 3];
      a[i * 8 + 4] = c[i * 8 + 4] + d[i * 8 + 4]; a[i * 8 + 5] = c[i * 8 + 4] + d[i * 8 + 5];
      a[i * 8 + 6] = c[i * 8 + 6] + d[i * 8 + 6];
      a[i * 8 + 7] = c[i * 8 + 6] + d[i * 8 + 7];
    }
}

void __attribute__ ((noipa))
f_golden (uint8_t *restrict a, uint8_t *restrict c, uint8_t *restrict d, int n)
{
#pragma GCC novector
  for (int i = 0; i < n; ++i)
    {
      a[i * 8] = c[i * 8] + d[i * 8];
      a[i * 8 + 1] = c[i * 8] + d[i * 8 + 1];
      a[i * 8 + 2] = c[i * 8 + 2] + d[i * 8 + 2];
      a[i * 8 + 3] = c[i * 8 + 2] + d[i * 8 + 3];
      a[i * 8 + 4] = c[i * 8 + 4] + d[i * 8 + 4];
      a[i * 8 + 5] = c[i * 8 + 4] + d[i * 8 + 5];
      a[i * 8 + 6] = c[i * 8 + 6] + d[i * 8 + 6];
      a[i * 8 + 7] = c[i * 8 + 6] + d[i * 8 + 7];
    }
}

#define LIMIT 256
#define NUM 32

int
main (void)
{
  uint8_t a[NUM * 8 + 8] = {0};
  uint8_t a_golden[NUM * 8 + 8] = {0};
  uint8_t c[NUM * 8 + 8] = {0};
  uint8_t d[NUM * 8 + 8] = {0};

  for (int i = 0; i < NUM * 8 + 8; i++)
    {
      if (i % NUM == 0)
	c[i] = (i + NUM) % LIMIT;
      else
	c[i] = (i * 3) % LIMIT;
      if (i % 2 == 0)
	d[i] = i % LIMIT;
      else
	d[i] = (i * 7) % LIMIT;
    }

  f (a, c, d, NUM);
  f_golden (a_golden, c, d, NUM);

  if (a[241] != 103)
    __builtin_abort ();

  return 0;
}
