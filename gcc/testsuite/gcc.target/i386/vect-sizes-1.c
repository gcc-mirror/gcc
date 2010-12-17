/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx -mtune=generic" } */

double a[1024];

void dependence_distance_4 (void)
{
  int i;
  for (i = 0; i < 1020; ++i)
    a[i + 4] = a[i] + a[i + 4];
}

/* { dg-final { scan-assembler "vmovapd\[ \\t\]+\[^\n\]*%ymm" } } */
