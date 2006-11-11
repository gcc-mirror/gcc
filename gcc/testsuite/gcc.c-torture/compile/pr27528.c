/* Check that constant constraints like "i", "n" and "s" can be used in
   cases where the operand is an initializer constant.  */
int x[2] = { 1, 2 };

#ifdef __OPTIMIZE__
static inline void __attribute__((__always_inline__))
insn1 (int x)
{
  asm volatile ("# %0 %1" :: "n" (x), "i" (x));
}

static inline void __attribute__((__always_inline__))
insn2 (const void *x)
{
  asm volatile ("# %0 %1" :: "s" (x), "i" (x));
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
  asm volatile ("# %0 %1" :: "s" (x), "i" (x));
  /* At the time of writing, &x[1] is decomposed before reaching expand
     when compiling with -O0.  */
  asm volatile ("# %0 %1" :: "s" ("string"), "i" ("string"));
  asm volatile ("# %0 %1" :: "s" (__FILE__), "i" (__FILE__));
  asm volatile ("# %0 %1" :: "s" (__FUNCTION__), "i" (__FUNCTION__));
}
