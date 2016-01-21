/* Verify that we don't stack a frame for leaf functions when using
   -pg -mprofile-kernel.  */

/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-options "-O2 -pg -mprofile-kernel" } */
/* { dg-final { scan-assembler-not "mtlr" } } */

int foo(void)
{
  return 1;
}
