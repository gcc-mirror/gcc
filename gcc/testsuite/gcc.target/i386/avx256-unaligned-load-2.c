/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load" } */

#define N 1024

char **ep;
char **fp;

void
avx_test (void)
{
  int i;
  char **ap;
  char **bp;
  char **cp;

  ap = ep;
  bp = fp;
  for (i = 128; i >= 0; i--)
    {
      *ap++ = *cp++;
      *bp++ = 0;
    }
}

/* { dg-final { scan-assembler-not "\\*avx_movdqu256/1" } } */
/* { dg-final { scan-assembler "\\*sse2_movdqu/1" } } */
/* { dg-final { scan-assembler "vinsert.128" } } */
