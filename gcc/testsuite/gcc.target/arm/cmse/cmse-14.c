/* { dg-do compile } */
/* { dg-options "-mcmse" } */


int __attribute__ ((cmse_nonsecure_call)) (*bar) (void);

int foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
/* { dg-final { scan-assembler-not "^(.*\\s)?bl?\[^\\s]*\\s+bar" } } */
