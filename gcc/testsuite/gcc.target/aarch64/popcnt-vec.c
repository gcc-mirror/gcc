/* { dg-do compile } */
/* { dg-options "-O2 -fno-vect-cost-model" } */

/* This function should produce cnt v.16b.  */
void
bar (unsigned char *__restrict b, unsigned char *__restrict d)
{
  for (int i = 0; i < 1024; i++)
    d[i] = __builtin_popcount (b[i]);
}

/* This function should produce cnt v.16b and uaddlp (Add Long Pairwise).  */
void
bar1 (unsigned short *__restrict b, unsigned short *__restrict d)
{
  for (int i = 0; i < 1024; i++)
    d[i] = __builtin_popcount (b[i]);
}

/* This function should produce cnt v.16b and 2 uaddlp (Add Long Pairwise).  */
void
bar2 (unsigned int *__restrict b, unsigned int *__restrict d)
{
  for (int i = 0; i < 1024; i++)
    d[i] = __builtin_popcount (b[i]);
}

/* This function should produce cnt v.16b and 3 uaddlp (Add Long Pairwise).  */
void
bar3 (unsigned long long *__restrict b, unsigned long long *__restrict d)
{
  for (int i = 0; i < 1024; i++)
    d[i] = __builtin_popcountll (b[i]);
}

/* SLP
   This function should produce cnt v.8b and uaddlp (Add Long Pairwise).  */
void
bar4 (unsigned short *__restrict b, unsigned short *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
  d[2] = __builtin_popcount (b[2]);
  d[3] = __builtin_popcount (b[3]);
}

/* SLP
   This function should produce cnt v.8b and 2 uaddlp (Add Long Pairwise).  */
void
bar5 (unsigned int *__restrict b, unsigned int *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
}

/* SLP
   This function should produce cnt v.16b and 3 uaddlp (Add Long Pairwise).  */
void
bar6 (unsigned long long *__restrict b, unsigned long long *__restrict d)
{
  d[0] = __builtin_popcountll (b[0]);
  d[1] = __builtin_popcountll (b[1]);
}

/* { dg-final { scan-assembler-not {\tbl\tpopcount} } } */
/* { dg-final { scan-assembler-times {cnt\t} 7 } } */
/* { dg-final { scan-assembler-times {uaddlp\t} 12 } } */
/* { dg-final { scan-assembler-times {ldr\tq} 5 } } */
/* { dg-final { scan-assembler-times {ldr\td} 2 } } */
