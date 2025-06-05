/* PR tree-optimization/120231 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64 } */
/* { dg-final { scan-tree-dump-not "link_failure \\\(\\\);" "optimized" } } */

void link_failure (void);

void
foo (long long x)
{
  _Float64 y = x;
  if (y >= -8577328745032543176.25f64 && y <= 699563045341050951.75f64)
    {
      if (x < -8577328745032544256LL || x > 699563045341051136LL)
	link_failure ();
    }
  if (y >= -49919160463252.125f64 && y <= 757060336735329.625f64)
    {
      if (x < -49919160463252LL || x > 757060336735329LL)
	link_failure ();
    }
}

void
bar (_Float64 x)
{
  long long y = x;
  if (y >= -6923230004751524066LL && y <= 2202103129706786704LL)
    {
      if (x < -6923230004751524864.0f64 || x > 2202103129706786816.0f64)
	link_failure ();
    }
  if (y >= -171621738469699LL && y <= 45962470357748LL)
    {
      if (x <= -1716217384696970.f64 || x >= 45962470357749.0f64)
	link_failure ();
    }
}
