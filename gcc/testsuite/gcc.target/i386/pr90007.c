/* PR rtl-optimization/90007 */
/* { dg-do compile } */
/* { dg-options "-march=bdver1 -mfpmath=387 -O1 -fschedule-insns -fselective-scheduling" } */

void
qj (int b9, int r9, int k4, int k0, int e7)
{
  (void) b9;
  (void) r9;
  (void) k4;

  while (!!k0 == e7 * 1.1)
    {
    }
}
