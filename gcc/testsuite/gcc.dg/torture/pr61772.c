/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { scan-assembler-times "XXX" 2 } } */

static inline __attribute__((always_inline)) int dec_and_test (int *i)
{
    asm volatile goto ("XXX %0, %l[cc_label]"
		       : : "m" (*i) : "memory" : cc_label);
    return 0;
cc_label:
    return 1;
}
extern int getit (int *);
int f (int *i, int cond)
{
  if (cond) {
      getit (0);
      if (dec_and_test (i))
	getit (i);
      return 42;
  }
  if (dec_and_test (i))
    (void)1;
  return getit (i);
}
