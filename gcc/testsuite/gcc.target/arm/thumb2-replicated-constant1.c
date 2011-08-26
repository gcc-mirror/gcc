/* Ensure simple replicated constant immediates work.  */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
foo1 (int a)
{
  return a + 0xfefefefe;
}

/* { dg-final { scan-assembler "add.*#-16843010" } } */

int
foo2 (int a)
{
  return a - 0xab00ab00;
}

/* { dg-final { scan-assembler "sub.*#-1426019584" } } */

int
foo3 (int a)
{
  return a & 0x00cd00cd;
}

/* { dg-final { scan-assembler "and.*#13435085" } } */
