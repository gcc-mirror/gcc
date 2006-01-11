/* { dg-do compile } */
/* { dg-options "-O1 -funsafe-math-optimizations -fdump-tree-recip" } */

float e(float a, float b, float c, float d, float e, float f)
{
  if (a < b)
    {
      a = a + b;
      c = c + d;
    }

  /* The PHI nodes for these divisions should be combined.  */
  d = d / a;
  e = e / a;
  f = f / a;
  
  a = a / c;
  b = b / c;

  /* This should not be left as a multiplication.  */
  c = 1 / c;

  return a + b + c + d + e + f;
}

/* { dg-final { scan-tree-dump-times " / " 2 "recip" } } */
/* { dg-final { scan-tree-dump-times " \\* " 5 "recip" } } */
/* { dg-final { cleanup-tree-dump "recip" } } */
