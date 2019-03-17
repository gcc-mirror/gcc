/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-maddress-mode=short -mx32 -Ofast -funroll-loops -march=haswell" } */
/* { dg-final { scan-assembler-not "\tvgather" } } */
/* { dg-final { scan-assembler "addr32 vgather" } } */

void foo (void);

extern float *ncost;

float
bar (int type, int num)
{
  int i;
  float cost;

  cost = 0;
  for (i = 0; i < num; i++)
    if (type)
      cost += ncost[i];
    else
      foo ();
  return (cost);
}
