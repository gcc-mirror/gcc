/* PR c/94722 */
/* Test that stack protection is disabled via no_stack_protector attribute. */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fstack-protector-all" } */

/* { dg-do compile { target { ! hppa*-*-* } } } */

int __attribute__((no_stack_protector)) c()
{
  int a;
  char b[34];
  return 0;
}

/* { dg-final { scan-assembler-not "stack_chk_fail" } } */
