/* { dg-do compile } */
/* { dg-options "-mcmse" }  */

struct span {
  int a, b;
};

extern int qux (void);

void __attribute__ ((cmse_nonsecure_entry))
foo (void) {}

static void __attribute__ ((cmse_nonsecure_entry))
bar (void) {} /* { dg-warning "has no effect on functions with static linkage" } */

int __attribute__ ((cmse_nonsecure_entry))
baz (void)
{
  return qux ();
}

/* { dg-final { scan-assembler-times "bxns" 2 } } */
/* { dg-final { scan-assembler "foo:" } } */
/* { dg-final { scan-assembler "__acle_se_foo:" } } */
/* { dg-final { scan-assembler-not "__acle_se_bar:" } } */
/* { dg-final { scan-assembler "baz:" } } */
/* { dg-final { scan-assembler "__acle_se_baz:" } } */
