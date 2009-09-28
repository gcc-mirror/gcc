/* { dg-do compile } */
/* { dg-options "-O1 -fno-trapping-math -fno-finite-math-only -fdump-tree-optimized" } */
  
extern void f(int);
extern void link_error ();

extern float x;
extern double y;
extern long double z;

int
main ()
{
  double pinf = __builtin_inf ();
#ifndef __SPU__
  /* The SPU single-precision floating point format does not support Inf.  */
  float pinff = __builtin_inff ();
#endif
  long double pinfl = __builtin_infl ();

  if (__builtin_isinf (pinf) != 1)
    link_error ();
#ifndef __SPU__
  if (__builtin_isinf (pinff) != 1)
    link_error ();
  if (__builtin_isinff (pinff) != 1)
    link_error ();
#endif
  if (__builtin_isinf (pinfl) != 1)
    link_error ();
  if (__builtin_isinfl (pinfl) != 1)
    link_error ();

  if (__builtin_isinf_sign (-pinf) != -1)
    link_error ();
#ifndef __SPU__
  if (__builtin_isinf_sign (-pinff) != -1)
    link_error ();
#endif
  if (__builtin_isinf_sign (-pinfl) != -1)
    link_error ();

  if (__builtin_isinf (4.0))
    link_error ();
  if (__builtin_isinf (4.0))
    link_error ();
  if (__builtin_isinff (4.0))
    link_error ();
  if (__builtin_isinf (4.0))
    link_error ();
  if (__builtin_isinfl (4.0))
    link_error ();
}


/* Check that all instances of link_error were subject to DCE.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
