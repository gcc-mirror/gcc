/* PR rtl-optimization/97684 */
/* { dg-do compile } */
/* { dg-options "-O1 -flive-range-shrinkage -fschedule-insns -fselective-scheduling -funroll-all-loops -fno-web" } */

void
c5 (double);

void
g4 (int *n4)
{
  double lp = 0.0;
  int fn;

  for (fn = 0; fn < 18; ++fn)
    {
      int as;

      as = __builtin_abs (n4[fn]);
      if (as > lp)
        lp = as;
    }

  c5 (lp);
}
