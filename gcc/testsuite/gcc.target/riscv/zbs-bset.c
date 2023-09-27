/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* bset */
long
sub0 (long i, long j)
{
  return i | (1L << j);
}

/* bset_mask */
long
sub1 (long i, long j)
{
  return i | (1L << (j & 0x3f));
}

/* bset_1 */
long
sub2 (long i)
{
  return 1L << i;
}

/* bset_1_mask */
long
sub3 (long i)
{
  return 1L << (i & 0x3f);
}

/* bseti */
long
sub4 (long i)
{
  return i | (1L << 20);
}

/* { dg-final { scan-assembler-times "bset\t" 4 } } */
/* { dg-final { scan-assembler-times "bseti\t" 1 } } */
/* { dg-final { scan-assembler-not {\mandi} } } */
