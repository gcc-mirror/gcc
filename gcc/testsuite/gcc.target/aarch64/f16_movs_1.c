/* { dg-do run } */
/* { dg-options "-fno-inline -O2" } */

#include <arm_neon.h>

__fp16
func2 (__fp16 a, __fp16 b)
{
  return b;
}

int
main (int argc, char **argv)
{
  __fp16 array[16];
  int i;

  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = i;

  array[0] = func2 (array[1], array[2]);

  __builtin_printf ("%f\n", array[0]); /* { dg-output "2.0" } */

  return 0;
}
