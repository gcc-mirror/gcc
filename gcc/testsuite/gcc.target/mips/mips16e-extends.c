/* -mlong32 added because of PR target/38595.  */
/* { dg-options "(-mips16) isa_rev>=1 -mlong32" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

extern int validate ();

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

/* { dg-final { scan-assembler "\tzeh\t" } } */
/* { dg-final { scan-assembler "\tseh\t" } } */
/* { dg-final { scan-assembler "\tzeb\t" } } */
/* { dg-final { scan-assembler "\tseb\t" } } */
