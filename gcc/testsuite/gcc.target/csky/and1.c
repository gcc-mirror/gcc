/* { dg-do compile } */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

int and1 (int x)
{
  return x & 0xfff7ffff;
}

/* { dg-final { scan-assembler "bclri" } } */

