/* { dg-require-effective-target loongarch_sx_hw } */
/* { dg-do run } */
/* { dg-options "-march=loongarch64 -mfpu=64 -mlsx -O3" } */

typedef __INT32_TYPE__ int32_t;
typedef unsigned __INT32_TYPE__ uint32_t;

__attribute__ ((noipa)) static int32_t
long_filter_ehigh_3830_1 (int32_t *buffer, int length)
{
  int i, j;
  int32_t dotprod = 0;
  int32_t delay[4] = { 0 };
  uint32_t coeffs[4] = { 0 };

  for (i = 0; i < length; i++)
    {
      dotprod = 0;
      for (j = 3; j >= 0; j--)
        {
          dotprod += delay[j] * coeffs[j];
          coeffs[j] += ((delay[j] >> 31) | 1);
        }
      for (j = 3; j > 0; j--)
        delay[j] = delay[j - 1];
      delay[0] = buffer[i];
    }

  return dotprod;
}

int
main ()
{
  int32_t buffer[] = { -1, 1 };
  if (long_filter_ehigh_3830_1 (buffer, 2) != -1)
    __builtin_trap ();
}
