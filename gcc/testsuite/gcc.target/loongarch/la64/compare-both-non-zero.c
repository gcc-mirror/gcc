/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3" } */

int
test (int a, int b)
{
  return a && b;
}

/* { dg-final { scan-assembler "maskeqz" } } */
