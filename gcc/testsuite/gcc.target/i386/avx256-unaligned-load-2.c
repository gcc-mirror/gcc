/* { dg-do compile { target { ! ia32 } } } */
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

/* { dg-final { scan-assembler-not "avx_loaddqu256" } } */
/* { dg-final { scan-assembler "sse2_loaddqu" } } */
/* { dg-final { scan-assembler "vinsert.128" } } */
