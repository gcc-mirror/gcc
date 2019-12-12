/* { dg-do compile } */
/* { dg-skip-if "Testing exclusion of -mcmse" { arm-*-* } { "-mcmse" } { "" } }  */


void __attribute__ ((cmse_nonsecure_call)) (*bar) (int); /* { dg-warning "attribute ignored without '-mcmse' option" } */
typedef void __attribute__ ((cmse_nonsecure_call)) baz (int); /* { dg-warning "attribute ignored without '-mcmse' option" } */

int __attribute__ ((cmse_nonsecure_entry))
foo (int a, baz b)
{ /* { dg-warning "attribute ignored without '-mcmse' option" } */
  bar (a);
  b (a);
  return a + 1;
}

/* { dg-final { scan-assembler-not "bxns" } } */
/* { dg-final { scan-assembler-not "blxns" } } */
/* { dg-final { scan-assembler-not "bl\t__gnu_cmse_nonsecure_call" } } */
/* { dg-final { scan-assembler "foo:" } } */
/* { dg-final { scan-assembler-not "__acle_se_foo:" } } */
