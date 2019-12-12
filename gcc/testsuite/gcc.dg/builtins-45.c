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
  float nanf = __builtin_nanf ("");
  long double nanl = __builtin_nanl ("");

  double pinf = __builtin_inf ();
  float pinff = __builtin_inff ();
  long double pinfl = __builtin_infl ();

  if (__builtin_finite (pinf))
    link_error ();
  if (__builtin_finitef (pinff))
    link_error ();
  if (__builtin_finitel (pinfl))
    link_error ();

  if (__builtin_finite (nan))
    link_error ();
  if (__builtin_finitef (nanf))
    link_error ();
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
