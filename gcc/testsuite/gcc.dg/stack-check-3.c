/* The goal here is to ensure that dynamic allocations via vlas or
   alloca calls receive probing.

   Scanning the RTL or assembly code seems like insanity here as does
   checking for particular allocation sizes and probe offsets.  For
   now we just verify that there's an allocation + probe loop and
   residual allocation + probe for f?.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-expand -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=4096 --param stack-clash-protection-guard-size=4096" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

__attribute__((noinline, noclone)) void
foo (char *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

/* Simple VLA, no other locals. */
__attribute__((noinline, noclone)) void
f0 (int x)
{
  char vla[x];
  foo (vla);
}

/* Simple VLA, small local frame.  */
__attribute__((noinline, noclone)) void
f1 (int x)
{
  char locals[128];
  char vla[x];
  foo (vla);
}

/* Small constant alloca, no other locals. */
__attribute__((noinline, noclone)) void
f2 (int x)
{
  char *vla = __builtin_alloca (128);
  foo (vla);
}

/* Big constant alloca, small local frame.  */
__attribute__((noinline, noclone)) void
f3 (int x)
{
  char locals[128];
  char *vla = __builtin_alloca (16384);
  foo (vla);
}

/* Big constant alloca, small local frame.  */
__attribute__((noinline, noclone)) void
f3a (int x)
{
  char locals[128];
  char *vla = __builtin_alloca (32768);
  foo (vla);
}

/* Nonconstant alloca, no other locals. */
__attribute__((noinline, noclone)) void
f4 (int x)
{
  char *vla = __builtin_alloca (x);
  foo (vla);
}

/* Nonconstant alloca, small local frame.  */
__attribute__((noinline, noclone)) void
f5 (int x)
{
  char locals[128];
  char *vla = __builtin_alloca (x);
  foo (vla);
}

/* { dg-final { scan-rtl-dump-times "allocation and probing residuals" 7 "expand" } } */


/* { dg-final { scan-rtl-dump-times "allocation and probing in loop" 7 "expand" { target callee_realigns_stack } } } */
/* { dg-final { scan-rtl-dump-times "allocation and probing in loop" 4 "expand" { target { ! callee_realigns_stack } } } } */
/* { dg-final { scan-rtl-dump-times "allocation and probing in rotated loop" 1 "expand" { target { ! callee_realigns_stack } } } } */
/* { dg-final { scan-rtl-dump-times "allocation and probing inline" 1 "expand" { target { ! callee_realigns_stack } } } } */
/* { dg-final { scan-rtl-dump-times "skipped dynamic allocation and probing loop" 1 "expand" { target { ! callee_realigns_stack } } } } */
