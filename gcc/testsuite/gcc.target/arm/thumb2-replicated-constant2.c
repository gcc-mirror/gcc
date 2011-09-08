/* Ensure split constants can use replicated patterns.  */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
foo1 (int a)
{
  return a + 0xfe00fe01;
}

/* { dg-final { scan-assembler "add.*#-33489408" } } */
/* { dg-final { scan-assembler "add.*#1" } } */

int
foo2 (int a)
{
  return a + 0xdd01dd00;
}

/* { dg-final { scan-assembler "add.*#-587145984" } } */
/* { dg-final { scan-assembler "add.*#65536" } } */

int
foo3 (int a)
{
  return a + 0x00443344;
}

/* { dg-final { scan-assembler "add.*#4456516" } } */
/* { dg-final { scan-assembler "add.*#13056" } } */

int
foo4 (int a)
{
  return a + 0x77330033;
}

/* { dg-final { scan-assembler "add.*#1996488704" } } */
/* { dg-final { scan-assembler "add.*#3342387" } } */

int
foo5 (int a)
{
  return a + 0x11221122;
}

/* { dg-final { scan-assembler "add.*#285217024" } } */
/* { dg-final { scan-assembler "add.*#2228258" } } */

int
foo6 (int a)
{
  return a + 0x66666677;
}

/* { dg-final { scan-assembler "add.*#1717986918" } } */
/* { dg-final { scan-assembler "add.*#17" } } */

int
foo7 (int a)
{
  return a + 0x99888888;
}

/* { dg-final { scan-assembler "add.*#-2004318072" } } */
/* { dg-final { scan-assembler "add.*#285212672" } } */

int
foo8 (int a)
{
  return a + 0xdddddfff;
}

/* { dg-final { scan-assembler "add.*#-572662307" } } */
/* { dg-final { scan-assembler "addw.*#546" } } */
