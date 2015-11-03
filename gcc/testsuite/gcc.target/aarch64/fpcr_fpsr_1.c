/* { dg-do compile } */
/* { dg-options "-O2" } */

void
f1 (int *x)
{
  __builtin_aarch64_set_fpsr (*x);
}

void
f2 (int *x)
{
  __builtin_aarch64_set_fpcr (*x);
}

void
f3 (int *x)
{
  *x = __builtin_aarch64_get_fpsr ();
}

void
f4 (int *x)
{
  *x = __builtin_aarch64_get_fpcr ();
}
