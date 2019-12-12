/* { dg-do compile } */
/* { dg-final { scan-assembler-not "ashrdi3" } } */
/* { dg-final { scan-assembler "__mspabi_srall" } } */

long long
foo (long long a)
{
  return a >> 4;
}

