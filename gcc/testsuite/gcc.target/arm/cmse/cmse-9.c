/* { dg-do compile } */
/* { dg-skip-if "Testing exclusion of -mcmse" { arm-*-* } { "-mcmse" } { "" } }  */


int __attribute__ ((cmse_nonsecure_entry))
foo (int a)
{ /* { dg-warning "attribute ignored without -mcmse option" } */
  return a + 1;
}

/* { dg-final { scan-assembler "foo:" } } */
/* { dg-final { scan-assembler-not "__acle_se_foo:" } } */
