/* { dg-do compile } */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

long long int land2 (long long int x)
{
  return x & 0x00000000ffffffffLL;
}

/* { dg-final { scan-assembler "movi.*, 0" } } */
