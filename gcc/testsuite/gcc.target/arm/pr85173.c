/* PR target/85173.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-probe-interval=14" } */
/* { dg-require-effective-target arm_thumb2_ok } */

__attribute__((noinline, noclone)) void
foo (char *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

/* Nonconstant alloca, small local frame.  */
__attribute__((noinline, noclone)) void
f5 (int x)
{
  char locals[128];
  char *vla = __builtin_alloca (x);
  foo (vla);
}
