/* PR c/94722 */
/* Test that stack protection is disabled via no_stack_protector attribute. */

/* { dg-do compile } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-options "-O2 -fstack-protector-all" } */

int __attribute__((no_stack_protector)) c()
{
  int a;
  char b[34];
  return 0;
}

/* { dg-final { scan-assembler-not "stack_chk_fail" } } */
