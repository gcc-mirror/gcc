/* { dg-do compile } */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

int or1 (int x)
{
  return x | 0x00100000;
}

/* { dg-final { scan-assembler "bseti" } } */
