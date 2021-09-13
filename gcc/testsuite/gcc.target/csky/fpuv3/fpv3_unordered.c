/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

int func32(float a, float b)
{
  return __builtin_isunordered(a, b);
}

int func64(double a, double b)
{
  return __builtin_isunordered(a, b);
}

/* { dg-final { scan-assembler "fcmpuo\.32" } }*/
/* { dg-final { scan-assembler "fcmpuo\.64" } }*/

int func32z(float a)
{
  return __builtin_isunordered(a, 0);
}

int func64z(double a)
{
  return __builtin_isunordered(a, 0);
}

/* { dg-final { scan-assembler "fcmpuoz\.32" } }*/
/* { dg-final { scan-assembler "fcmpuoz\.64" } }*/
