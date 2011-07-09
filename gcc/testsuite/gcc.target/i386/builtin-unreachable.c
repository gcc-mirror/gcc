/* This should return 1 without setting up a stack frame or
   jumping.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
int h (char *p)
{
  if (*p)
    __builtin_unreachable ();
  return p ? 1 : 0;
}
/* { dg-final { scan-assembler-not "%e\[bs\]p" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+j" } } */
