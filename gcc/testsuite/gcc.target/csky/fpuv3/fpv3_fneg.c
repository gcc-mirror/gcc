/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-mfpu=fpv3" } */

__fp16 func16(__fp16 a)
{
  return -a;
}

float func32(float a)
{
  return -a;
}

double func64(double a)
{
  return -a;
}

/* { dg-final { scan-assembler "fneg\.32" } }*/
/* { dg-final { scan-assembler "fneg\.64" } }*/

