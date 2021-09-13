/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-mfpu=fpv3" } */

__fp16 funch(__fp16 a, __fp16 b)
{
  return a - b;
}

float funcs(float a, float b)
{
  return a - b;
}

double funcd(double a, double b)
{
  return a - b;
}

/* { dg-final { scan-assembler "fsub\.16" } }*/
/* { dg-final { scan-assembler "fsub\.32" } }*/
/* { dg-final { scan-assembler "fsub\.64" } }*/

