/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck801"  { csky-*-* }  { "*" }  { "-mcpu=ck801" }  }  */
/* { dg-csky-options "-O1" } */

/* Test special code generation patterns for bit operators.  */

int and3 (int x)
{
  return x & 0x000fffff;
}

/* { dg-final { scan-assembler "lsli" } } */
/* { dg-final { scan-assembler "lsri" } } */
