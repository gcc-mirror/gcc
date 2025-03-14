/* PR target/119120 */
/* { dg-do compile } */
/* { dg-options "-O0 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "REALPART_EXPR <r> = " "optimized" } } */
/* { dg-final { scan-tree-dump "IMAGPART_EXPR <r> = " "optimized" } } */
/* { dg-final { scan-tree-dump "REALPART_EXPR <s> = " "optimized" } } */
/* { dg-final { scan-tree-dump-not "(REAL|IMAG)PART_EXPR <t> = " "optimized" } } */
/* { dg-final { scan-tree-dump-not "(REAL|IMAG)PART_EXPR <u> = " "optimized" } } */

__complex__ double
foo (void)
{
  __complex__ double r;
  __imag__ r = 2.0;
  __real__ r = 1.0;
  return r + 1.0;
}

__complex__ float
bar (float x, float y)
{
  __complex__ float s = x + y * 1.0fi;
  __real__ s = 1.0f;
  return s + 1.0f;
}

__complex__ float
baz (float x, float y)
{
  __complex__ float t = x + y * 1.0fi;
  return t + 1.0f;
}

__complex__ float
qux (__complex__ float x)
{
  __complex__ float u;
  u = x;
  return u + 1.0f;
}
