/* Check that attribute target arm is rejected for M profile.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6-m" } } */
/* { dg-add-options arm_arch_v6m } */

int __attribute__((target("arm")))
foo(int a)
{  /* { dg-error "does not support" } */
  return a ? 1 : 5;
}


