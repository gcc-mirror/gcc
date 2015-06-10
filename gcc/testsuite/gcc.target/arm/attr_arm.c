/* Check that attribute target arm is recogniwed.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler ".arm" } } */
/* { dg-final { scan-assembler-not "ite" } } */

int __attribute__((target("arm")))
foo(int a)
{
  return a ? 1 : 5;
}

