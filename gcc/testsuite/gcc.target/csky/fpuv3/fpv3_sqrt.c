/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

float sqrtf(float);
float sqrt32(float a){
  return sqrtf(a);
}

double sqrt(double);
double sqrt64(double a){
  return sqrt(a);
}

/* { dg-final { scan-assembler "fsqrt\.32" } }*/
/* { dg-final { scan-assembler "fsqrt\.64" } }*/
