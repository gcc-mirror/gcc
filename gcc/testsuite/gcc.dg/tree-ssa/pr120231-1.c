/* PR tree-optimization/120231 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float64 } */
/* { dg-require-effective-target float128 } */
/* { dg-final { scan-tree-dump-not "link_failure \\\(\\\);" "optimized" } } */

void link_failure (void);

void
foo (_Float64 x)
{
  if (x >= -64.0f64 && x <= 0x1.p+140f64)
    {
      _Float32 z = x;
      _Float128 w = z;
      _Float128 v = x;
      if (__builtin_isnan (z)
	  || __builtin_isnan (w)
	  || __builtin_isnan (v)
	  || z < -64.0f32
	  || w < -64.0f128
	  || __builtin_isinf (v)
	  || v < -64.0f128
	  || v > 0x1.p+140f128)
	link_failure ();
    }
}

void
bar (_Float64 x)
{
  _Float32 z = x;
  if (z >= -64.0f32 && z <= 0x1.p+38f32)
    {
      if (__builtin_isnan (x)
	  || __builtin_isinf (x)
	  || x < -0x1.000001p+6f64
	  || x > 0x1.000001p+38f64)
	link_failure ();
    }
}

void
baz (_Float64 x)
{
  _Float128 w = x;
  if (w >= -64.0f128 && w <= 0x1.p+1026f128)
    {
      if (__builtin_isnan (x)
	  || __builtin_isinf (x)
	  || x < -64.0f64)
	link_failure ();
    }
  if (w >= 128.25f128 && w <= 0x1.p+1020f128)
    {
      if (__builtin_isnan (x)
	  || __builtin_isinf (x)
	  || x < 128.25f64
	  || x > 0x1.p+1020f64)
	link_failure ();
    }
}
