/* Check that constant constraints like "i", "n" and "s" can be used in
   cases where the operand is an initializer constant.  */
/* { dg-require-effective-target nonpic } */

int x[2] = { 1, 2 };

#ifdef __OPTIMIZE__
static inline void __attribute__((__always_inline__))
insn1 (int x)
{
  asm volatile ("" :: "n" (x), "i" (x));
}

static inline void __attribute__((__always_inline__))
insn2 (const void *x)
{
  asm volatile ("" :: "s" (x), "i" (x));
}
#endif

void
foo (void)
{
#ifdef __OPTIMIZE__
  insn1 (2);
  insn1 (2);
  insn1 (400);
  insn1 (__LINE__);
  insn2 (x);
  insn2 (x);
  insn2 (&x[1]);
  insn2 ("string");
#endif
  asm volatile ("" :: "s" (x), "i" (x));
  /* At the time of writing, &x[1] is decomposed before reaching expand
     when compiling with -O0.  */
  asm volatile ("" :: "s" ("string"), "i" ("string"));
  asm volatile ("" :: "s" (__FILE__), "i" (__FILE__));
  asm volatile ("" :: "s" (__FUNCTION__), "i" (__FUNCTION__));
}
