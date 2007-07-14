/* PR target/10114 */
/* Originator: James Troup <james@nocrew.org> */

/* { dg-do compile } */
/* { dg-options "-g -O1" } */

extern __inline double sqrt (double __x)
{
  register double __r;
  __asm ("fsqrtd %1,%0" : "=f" (__r) : "f" (__x));
  return __r;
}

static double our_skew, max_update_skew;

static double Sqr(double x)
{
  return x*x;
}

void REF_SetReference(double skew)
{
  double previous_skew, new_skew;
  double old_weight, new_weight, sum_weight;
  double delta_freq1, delta_freq2;
  double skew1, skew2;

  previous_skew = our_skew;
  skew1 = sqrt((Sqr(delta_freq1) * old_weight + Sqr(delta_freq2) * new_weight) / sum_weight);
  skew2 = (previous_skew * old_weight + new_skew * new_weight) / sum_weight;
  our_skew = skew1 + skew2;
}
