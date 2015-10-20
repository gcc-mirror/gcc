/* { dg-do compile } */
/* { dg-options "-Ofast" } */


typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

void
bar (int32x2_t *foo)
{
  int i = 0;
  int32x2_t val = { 3, 2 };

  for (i = 0; i < 256; i+=2)
    {
      foo[i] = val;
      foo[i+1] = val;
    }
}

/* { dg-final { scan-assembler "stp\td\[0-9\]+, d\[0-9\]" } } */
