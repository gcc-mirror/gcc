/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-mfpu=fpv3" } */

__fp16 func(float a)
{
  return (__fp16)a;
}

/* { dg-final { scan-assembler "fstoh" } }*/

