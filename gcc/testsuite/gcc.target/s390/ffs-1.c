/* Check that __builtin_ffs does not expand to libcall.  This is required by
   Linux kernel code since libcalls are not present there.  */
/* { dg-do compile } */
/* { dg-options "-march=z10" } */

long
fool (long x)
{
  return __builtin_ffsl (x);
}

int
foo (int x)
{
  return __builtin_ffs (x);
}

/* { dg-final { scan-assembler-not "brasl" } } */
