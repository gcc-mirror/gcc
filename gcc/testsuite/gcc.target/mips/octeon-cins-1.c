/* { dg-do compile } */
/* The tests also work with -mgp32.  For long long tests, only one of
   the 32-bit parts is used.  */
/* { dg-options "-march=octeon" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tcins\t" 3 } } */
/* { dg-final { scan-assembler-not "\tandi\t|sll\t" } } */

NOMIPS16 long long
f (long long i)
{
  return (i & 0xff) << 34;
}

NOMIPS16 int
g (int i)
{
  return (i << 4) & 0xff0;
}

NOMIPS16 long long
h (long long i)
{
  return (i << 4) & 0xfff;
}
