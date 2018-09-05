/* { dg-do compile } */
/* { dg-csky-options "-mcpu=ck810f -O1" } */

/* Test special code generation patterns for bit operators.  */

int and3 (int x)
{
  return x & 0x000fffff;
}

/* { dg-final { scan-assembler "zext" } } */
