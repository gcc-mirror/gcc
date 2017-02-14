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

void __attribute__ ((cmse_nonsecure_call))
quux (void) {} /* { dg-warning "attribute only applies to base type of a function pointer" } */

int __attribute__ ((cmse_nonsecure_call)) norf; /* { dg-warning "attribute only applies to base type of a function pointer" } */

/* { dg-final { scan-assembler-times "bxns" 2 } } */
/* { dg-final { scan-assembler "foo:" } } */
/* { dg-final { scan-assembler "__acle_se_foo:" } } */
/* { dg-final { scan-assembler-not "__acle_se_bar:" } } */
/* { dg-final { scan-assembler "baz:" } } */
/* { dg-final { scan-assembler "__acle_se_baz:" } } */
/* { dg-final { scan-assembler-not "__acle_se_quux:" } } */
/* { dg-final { scan-assembler-not "__acle_se_norf:" } } */
