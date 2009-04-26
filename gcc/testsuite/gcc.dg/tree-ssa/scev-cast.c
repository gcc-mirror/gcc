/* A test for various conversions of chrecs.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void blas (char xxx);
void blau (unsigned char xxx);

void tst(void)
{
  unsigned i;

  for (i = 0; i < 128; i++) /* This cast to char has to be preserved.  */
    blas ((char) i);
  for (i = 0; i < 127; i++) /* And this one does not.  */
    blas ((char) i);
  for (i = 0; i < 255; i++) /* This cast is not necessary.  */
    blau ((unsigned char) i);
  for (i = 0; i < 256; i++)
    blau ((unsigned char) i); /* This one is necessary.  */
}

/* { dg-final { scan-tree-dump-times "= \\(unsigned char\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \\(char\\)" 1 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
