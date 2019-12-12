/* { dg-do compile } */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

int and2 (int x)
{
  return x & 0xfff00000;
}

/* { dg-final { scan-assembler "lsri" } } */
/* { dg-final { scan-assembler "lsli" } } */
