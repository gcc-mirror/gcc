/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse" } */
/* { dg-require-effective-target ia32 } */
__attribute__((target ("sse2"), noinline)) static void
foo (void)
{
  asm volatile ("" : : : "memory");
}

void
bar (void)
{
  foo ();
}

