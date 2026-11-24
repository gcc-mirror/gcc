/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcbv_zicond_zvl128b -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2" "-Os" "-Og" } } */

/* The vectorizer guards this loop with a runtime alias check, which tests
   that neither address difference is within 12 bytes of zero.  That is
   !(A <=u 12) && !(B <=u 12), which should be computed as
   !(umin (A, B) <=u 12) and branched on directly.  */

void
f (short *a, short *b, short *c)
{
  for (int i = 0; i < 16; i++)
    a[i] = b[i] + c[i];
}

/* { dg-final { scan-assembler-times "\tminu\t" 1 } } */
/* { dg-final { scan-assembler-not "\tseqz\t" } } */
