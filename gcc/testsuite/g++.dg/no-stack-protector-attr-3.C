/* PR c/94722 */
/* Test that stack protection is disabled via no_stack_protector attribute. */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fstack-protector-explicit" } */

/* { dg-do compile { target { ! hppa*-*-* } } } */

int __attribute__((no_stack_protector)) foo()
{
  int a;
  char b[34];
  return 0;
}

int __attribute__((stack_protect)) bar()
{
  int a;
  char b[34];
  return 0;
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 1 { target { ! mips*-*-* } } } }*/
/* { dg-final { scan-assembler-times "stack_chk_fail" 2 { target { mips*-*-* } } } }*/
