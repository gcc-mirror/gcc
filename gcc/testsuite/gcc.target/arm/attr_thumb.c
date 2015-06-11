/* Check that attribute target thumb is recognized. */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler ".thumb" } } */
/* { dg-final { scan-assembler "ite" } } */

int __attribute__((target("thumb")))
foo(int a)
{
  return a ? 1 : 5;
}

