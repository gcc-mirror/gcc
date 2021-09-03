/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-vectorize -fno-tree-fre" } */
/* { dg-additional-options "-march=armv8-a+sve" { target aarch64-*-* } } */



signed char i;

void
foo (void)
{
  for (i = 0; i < 6; i += 5)
    ;
}
