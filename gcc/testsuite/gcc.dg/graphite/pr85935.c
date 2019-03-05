/* { dg-do compile } */
/* { dg-options "-O -floop-parallelize-all -fno-tree-loop-im --param scev-max-expr-size=3" } */
/* The fix for PR84204 was reverted.  */
/* { dg-additional-options "--param graphite-allow-codegen-errors=1" } */

typedef int dq;

int gb;

void
qq (dq ww, int kk)
{
  dq *up = &ww;

  (void) *up;

  while (kk < 1)
    {
      ++ww;

      if (ww == 0)
	for (gb = 0; gb < 2; ++gb)
	  ;

      ++kk;
    }
}
