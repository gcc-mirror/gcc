/* -mlong32 added because of PR target/38595.  */
/* { dg-options "(-mips16) -Os isa_rev>=1 -mlong32" } */

MIPS16 short cksum16 (unsigned long n)
{
  unsigned long l;
  l = validate (n, (n >> 16) + (n & 0xffff));
  return l;
}

MIPS16 signed char cksum8 (unsigned long n)
{
  unsigned long l;
  l = validate (n, (n >> 8) + (n & 0xff));
  return l;
}

/* { dg-final { scan-assembler "zeh" } } */
/* { dg-final { scan-assembler "seh" } } */
/* { dg-final { scan-assembler "zeb" } } */
/* { dg-final { scan-assembler "seb" } } */
