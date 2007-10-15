/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void noreturn (int status, ...);

extern inline __attribute ((always_inline)) void
error (int status, ...)
{
  if (__builtin_constant_p (status))
    noreturn (status, __builtin_va_arg_pack ());
}

void
f (void)
{
  error (1);
}
