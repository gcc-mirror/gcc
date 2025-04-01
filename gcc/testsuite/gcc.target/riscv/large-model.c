/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -fno-section-anchors -mcmodel=large -fno-pie" } */
/* { dg-skip-if "" { *-*-* } {"-O0"} } */
int a, b;
int foo1()
{
  return a*b;
}

/* { dg-final { scan-assembler-times "ld.*LC0" 1 } } */
/* { dg-final { scan-assembler-times "ld.*LC1" 1 } } */
