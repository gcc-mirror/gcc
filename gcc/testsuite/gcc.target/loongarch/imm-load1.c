/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler "test:.*lu52i\.d.*\n\taddi\.w.*\n\.L2:" } } */


extern long long b[10];
static inline long long
repeat_bytes (void)
{
  long long r = 0x0101010101010101;

  return r;
}

static inline long long
highbit_mask (long long m)
{
  return m & repeat_bytes ();
}

void test(long long *a)
{
  for (int i = 0; i < 10; i++)
    b[i] = highbit_mask (a[i]);

}
