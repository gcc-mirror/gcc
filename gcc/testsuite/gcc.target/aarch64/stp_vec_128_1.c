/* { dg-do compile } */
/* { dg-options "-Ofast -moverride=tune=none" } */


typedef int int32x4_t __attribute__ ((__vector_size__ ((16))));

void
bar (int32x4_t *foo)
{
  int i = 0;
  int32x4_t val = { 3, 2, 5, 1 };

  for (i = 0; i < 256; i+=2)
    {
      foo[i] = val;
      foo[i+1] = val;
    }
}

/* { dg-final { scan-assembler "stp\tq\[0-9\]+, q\[0-9\]" } } */
