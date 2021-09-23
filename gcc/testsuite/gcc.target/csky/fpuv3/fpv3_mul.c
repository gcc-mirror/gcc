/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "" } */

float mul32(float a, float b){
  return a*b;
}

double mul64(double a, double b){
  return a*b;
}

/* { dg-final { scan-assembler "fmul\.32" } }*/
/* { dg-final { scan-assembler "fmul\.64" } }*/

