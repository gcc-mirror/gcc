/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tcins\t" } } */

NOMIPS16 unsigned
f (unsigned i)
{
  return (i & 0xff) << 24;
}

NOMIPS16 unsigned long long
g (unsigned long long i)
{
  return (i & 0x1ffffffffULL) << 4;
}
