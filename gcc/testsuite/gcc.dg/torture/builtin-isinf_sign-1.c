/* Copyright (C) 2008  Free Software Foundation.

   Verify that __builtin_isinf_sign folds correctly.

   Origin: Kaveh R. Ghazi,  May 17, 2008.  */

/* { dg-do link } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

void __attribute__ ((__noinline__))
foo (float f, double d, long double ld)
{
  /* Test the generic expansion of isinf_sign.  */

  if (__builtin_isinf_sign(f)
      != (__builtin_isinf(f) ? (__builtin_signbitf(f) ? -1 : 1) : 0))
    link_error (__LINE__);
  if (__builtin_isinf_sign(d)
      != (__builtin_isinf(d) ? (__builtin_signbit(d) ? -1 : 1) : 0))
    link_error (__LINE__);
  if (__builtin_isinf_sign(ld)
      != (__builtin_isinf(ld) ? (__builtin_signbitl(ld) ? -1 : 1) : 0))
    link_error (__LINE__);

#ifdef __OPTIMIZE__
  /* In boolean contexts, GCC will fold the inner conditional
     expression to 1.  So isinf_sign folds to plain isinf.  */

  if ((_Bool)__builtin_isinf_sign(f) != (__builtin_isinf(f) != 0))
    link_error (__LINE__);
  if ((_Bool)__builtin_isinf_sign(d) != (__builtin_isinf(d) != 0))
    link_error (__LINE__);
  if ((_Bool)__builtin_isinf_sign(ld) != (__builtin_isinf(ld) != 0))
    link_error (__LINE__);
#endif

  if ((__builtin_isinf_sign(f) != 0) != (__builtin_isinf(f) != 0))
    link_error (__LINE__);
  if ((__builtin_isinf_sign(d) != 0) != (__builtin_isinf(d) != 0))
    link_error (__LINE__);
  if ((__builtin_isinf_sign(ld) != 0) != (__builtin_isinf(ld) != 0))
    link_error (__LINE__);

  if ((__builtin_isinf_sign(f) ? 5 : 6) != (__builtin_isinf(f) ? 5 : 6))
    link_error (__LINE__);
  if ((__builtin_isinf_sign(d) ? 5 : 6) != (__builtin_isinf(d) ? 5 : 6))
    link_error (__LINE__);
  if ((__builtin_isinf_sign(ld) ? 5 : 6) != (__builtin_isinf(ld) ? 5 : 6))
    link_error (__LINE__);
}

int main (void)
{
  foo (1, 2, 3);
  return 0;
}
