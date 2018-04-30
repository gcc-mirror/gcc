/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short" } */

int
stackuse (void)
{
  volatile int foo = 2;
  return foo * 3;
}

/* Verify we that use %rsp to access stack.  */
/* { dg-final { scan-assembler-not "%esp" } } */
/* { dg-final { scan-assembler "%rsp" } } */
