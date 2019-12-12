/* { dg-do compile } */
/* { dg-final { scan-assembler-not "lshrdi3" } } */
/* { dg-final { scan-assembler "__mspabi_srlll" } } */

unsigned long long
foo (unsigned long long a)
{
  return a >> 4;
}

