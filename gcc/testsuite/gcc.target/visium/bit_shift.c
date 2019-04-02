/* { dg-do compile } */
/* { dg-options "-O" } */

int bit_shift (long int x)
{
  int i, n;

  for (i = n = 0; x && (i < (sizeof(long) * 8)); ++i, x >>= 1)
    n += (int)(x & 1L);
  return n;
}

/* { dg-final { scan-assembler-not "cmp" { xfail *-*-* } } } */
