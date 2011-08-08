/* A test for various conversions of chrecs.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void blas (signed char xxx);
void blau (unsigned char xxx);

void tst(void)
{
  unsigned i;

  for (i = 0; i < 129; i++) /* This truncation to char has to be preserved.  */
    blas ((signed char) i);
  for (i = 0; i < 128; i++) /* This one is not necessary, VRP eliminates it.  */
    blas ((signed char) i);
  for (i = 0; i < 127; i++) /* This one is not necessary, IVOPTS eliminates it.  */
    blas ((signed char) i);
  for (i = 0; i < 256; i++) /* This one is not necessary, VRP eliminates it.  */
    blau ((unsigned char) i);
  for (i = 0; i < 257; i++) /* This one is necessary.  */
    blau ((unsigned char) i);
}

/* { dg-final { scan-tree-dump-times "& 255" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \\(signed char\\)" 1 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
