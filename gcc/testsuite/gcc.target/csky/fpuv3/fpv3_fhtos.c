/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-mfpu=fpv3" } */

float func(__fp16 a)
{
  return (float)a;
}

/* { dg-final { scan-assembler "fhtos" } }*/

