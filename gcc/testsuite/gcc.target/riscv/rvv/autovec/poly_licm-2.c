/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

typedef unsigned short uint16_t;

void AAA (uint16_t *x, uint16_t *y, unsigned wsize, unsigned count)
{
  unsigned m = 0, n = count;
  register uint16_t *p;

  p = x;

  do {
    m = *--p;
    *p = (uint16_t)(m >= wsize ? m-wsize : 0);
  } while (--n);

  n = wsize;
  p = y;

  do {
      m = *--p;
      *p = (uint16_t)(m >= wsize ? m-wsize : 0);
  } while (--n);
}

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+\s+vrsub\.vx\s+} 2 } } */
