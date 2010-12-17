/* { dg-do link } */

extern void link_error (void);

int main()
{
  if (!__builtin_constant_p(1.0))
    link_error ();
  if (!__builtin_constant_p(__builtin_fma (1.0, 1.0, 1.0)))
    link_error ();

  if (__builtin_constant_p(__builtin_fmaf (__FLT_MAX__, __FLT_MAX__, 0.0F)))
    link_error ();
  if (__builtin_constant_p(__builtin_fmaf (__FLT_MAX__, 1.0F, __FLT_MAX__)))
    link_error ();
  if (__builtin_constant_p(__builtin_fmaf (__FLT_MIN__, __FLT_MIN__, 0.0F)))
    link_error ();

  if (__builtin_constant_p(__builtin_fma (__DBL_MAX__, __DBL_MAX__, 0.0)))
    link_error ();
  if (__builtin_constant_p(__builtin_fma (__DBL_MAX__, 1.0, __DBL_MAX__)))
    link_error ();
  if (__builtin_constant_p(__builtin_fma (__DBL_MIN__, __DBL_MIN__, 0.0)))
    link_error ();

  if (__builtin_constant_p(__builtin_fmal (__LDBL_MAX__, __LDBL_MAX__, 0.0L)))
    link_error ();
  if (__builtin_constant_p(__builtin_fmal (__LDBL_MAX__, 1.0L, __LDBL_MAX__)))
    link_error ();
  if (__builtin_constant_p(__builtin_fmal (__LDBL_MIN__, __LDBL_MIN__, 0.0L)))
    link_error ();

  return 0;
}
