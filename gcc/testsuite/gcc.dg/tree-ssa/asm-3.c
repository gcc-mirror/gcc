/* PR 17739 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#ifdef __hppa__
#define REGISTER "1"
#else
#ifdef __moxie__
#define REGISTER "8"
#else
#define REGISTER "0"
#endif
#endif

static inline int source(void)
{
  register int hardreg __asm__(REGISTER);
  asm("" : "=r"(hardreg));
  return hardreg;
}
                                                                                
void test(void)
{
  int t = source();
  foo(t);
  bar(t);
}

/* Hardreg should appear exactly 3 times -- declaration, asm stmt,
   and copy out.  */
/* { dg-final { scan-tree-dump-times "hardreg" 3 "optimized" } } */

/* In particular, hardreg should *not* appear in the call to bar.  */
/* { dg-final { scan-tree-dump-times "bar \[(\]\[^\n\r\]*_.\[)\]" 1 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
