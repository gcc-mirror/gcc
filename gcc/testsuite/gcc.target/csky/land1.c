/* { dg-do compile } */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

long long int land1 (long long int x)
{
  return x & 0xffffffff00000000LL;
}

/* { dg-final { scan-assembler "movi.*, 0" } } */
