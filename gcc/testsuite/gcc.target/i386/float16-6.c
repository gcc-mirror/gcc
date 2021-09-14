/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -mfpmath=sse -fdump-tree-gimple -fexcess-precision=16" } */
/* { dg-final { scan-tree-dump-not "\\(float\\)" "gimple" } } */
_Float16
foo (_Float16 a, _Float16 b, _Float16 c)
{
  return a + b + c;
}
