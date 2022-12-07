/* Test BTF generation for extern variables.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect 4 variables.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 4 } } */

/* 2 extern, 1 global, 1 static.  */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*btv_linkage" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*btv_linkage" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*btv_linkage" 2 } } */

extern int a;
extern const int b;
int c;
static const int d = 5;

int foo (int x)
{
  c = a + b + x;

  return c + d;
}
