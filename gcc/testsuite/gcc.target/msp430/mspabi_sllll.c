/* { dg-do compile } */
/* { dg-final { scan-assembler-not "ashldi3" } } */
/* { dg-final { scan-assembler "__mspabi_sllll" } } */

long long
foo (long long a)
{
  return a << 4;
}

