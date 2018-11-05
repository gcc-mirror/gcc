/* { dg-do compile } */
/* { dg-require-effective-target inf } */
/* { dg-options "-O1 -fno-trapping-math -fno-finite-math-only -fdump-tree-optimized" } */
  
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

  double pinf = __builtin_inf ();
#ifndef __SPU__
  /* The SPU single-precision floating point format does not support Inf.  */
  float pinff = __builtin_inff ();
#endif
  long double pinfl = __builtin_infl ();

  if (__builtin_finite (pinf))
    link_error ();
#ifndef __SPU__
  if (__builtin_finitef (pinff))
    link_error ();
#endif
  if (__builtin_finitel (pinfl))
    link_error ();

  if (__builtin_finite (nan))
    link_error ();
#ifndef __SPU__
  if (__builtin_finitef (nanf))
    link_error ();
#endif
  if (__builtin_finitel (nanl))
    link_error ();

  if (!__builtin_finite (4.0))
    link_error ();
  if (!__builtin_finitef (4.0))
    link_error ();
  if (!__builtin_finitel (4.0))
    link_error ();
}


/* Check that all instances of link_error were subject to DCE.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
