/* { dg-do compile } */
/* { dg-options "-O1 -fno-trapping-math -fno-finite-math-only -fdump-tree-gimple -fdump-tree-optimized" } */
  
extern void f(int);
extern void link_error ();

extern float x;
extern double y;
extern long double z;

int
main ()
{
  double nan = __builtin_nan ("");
#ifndef __SPU__
  /* The SPU single-precision floating point format does not support NANs.  */
  float nanf = __builtin_nanf ("");
#endif
  long double nanl = __builtin_nanl ("");

  if (!__builtin_isnan (nan))
    link_error ();
#ifndef __SPU__
  if (!__builtin_isnan (nanf))
    link_error ();
  if (!__builtin_isnanf (nanf))
    link_error ();
#endif
  if (!__builtin_isnan (nanl))
    link_error ();
  if (!__builtin_isnanl (nanl))
    link_error ();

  if (__builtin_isnan (4.0))
    link_error ();
  if (__builtin_isnan (4.0))
    link_error ();
  if (__builtin_isnanf (4.0))
    link_error ();
  if (__builtin_isnan (4.0))
    link_error ();
  if (__builtin_isnanl (4.0))
    link_error ();

  f (__builtin_isnan (x));
  f (__builtin_isnan (y));
  f (__builtin_isnanf (y));
  f (__builtin_isnan (z));
  f (__builtin_isnanl (z));
}


/* Check that all instances of __builtin_isnan were folded.  */
/* { dg-final { scan-tree-dump-times "isnan" 0 "gimple" } } */

/* Check that all instances of link_error were subject to DCE.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
