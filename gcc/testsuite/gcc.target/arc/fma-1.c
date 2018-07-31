/* { dg-do compile } */
/* { dg-skip-if "FPU not available" { arc700 || arc6xx } } */
/* { dg-options "-s -std=gnu11  -O2 -frounding-math -mfpu=fpus_all" } */

const float a, b = 7.8539818525e01;

/* Check if the fma operation is generated correctly.  */

int foo (void)
{
  return (float)3.0 * b + a;
}
/* { dg-final { scan-assembler "fsmadd" } } */
