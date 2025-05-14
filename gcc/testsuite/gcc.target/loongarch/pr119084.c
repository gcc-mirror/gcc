/* { dg-do run } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-require-effective-target loongarch_sx_hw } */

typedef signed char V16QI __attribute__ ((vector_size (16)));
static char x[128];

__attribute__ ((noipa)) int
noopt (int x)
{
  return x;
}

int
main (void)
{
  int t = noopt (32);

  x[32] = 1;

  V16QI y = __builtin_lsx_vldx (x, t);
  if (y[0] != 1)
    __builtin_trap ();
}
