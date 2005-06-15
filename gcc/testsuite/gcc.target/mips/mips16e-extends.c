/* { dg-do compile } */
/* { dg-mips-options "-Os -march=mips32 -mips16" } */

short cksum16 (unsigned long n)
{
  unsigned long l;
  l = validate (n, (n >> 16) + (n & 0xffff));
  return l;
}

char cksum8 (unsigned long n)
{
  unsigned long l;
  l = validate (n, (n >> 8) + (n & 0xff));
  return l;
}

/* { dg-final { scan-assembler "zeh" } } */
/* { dg-final { scan-assembler "seh" } } */
/* { dg-final { scan-assembler "zeb" } } */
/* { dg-final { scan-assembler "seb" } } */
