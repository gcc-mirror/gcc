/* { dg-do compile } */
/* { dg-options "-O2" } */
struct S { float a, b, c, d; };

int
bar (struct S x, struct S y)
{
  return x.b <= y.d && x.c >= y.a;
}

/* { dg-final { scan-assembler-not "movq" } } */
/* { dg-final { scan-assembler-not "xchgq" } } */
/* { dg-final { scan-assembler-not "shrq" } } */
/* { dg-final { scan-assembler-not "movd" } } */
