/* The and is performed in DI mode so there is no need for truncation.  */
/* { dg-options "-mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tsll\t" } } */

NOMIPS16 unsigned long long
f (unsigned long long s)
{
  unsigned u = s & 0xfff;
  return u;
}
