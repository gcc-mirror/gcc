/* Inverse of divmod-libcall-1.c. Ensure libcalls are generated for
   -mcpu=v3 due to lack of signed div/mod.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v3" } */
/* { dg-final { scan-assembler "global\t__divdi3" } } */
/* { dg-final { scan-assembler "global\t__moddi3" } } */

long
foo (long len)
{
  return len * 234 / 5;
}

long
bar (long len)
{
  return len * 234 % 5;
}
