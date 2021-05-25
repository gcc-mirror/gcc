/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

float fls32(float a, float b, float c, float d){
  if(a <= b)
    return c;

  return d;
}

double fhz64(double a, double b, double c, double d){
  if( a <= b)
    return c;
  return d;
}

/* { dg-final { scan-assembler "fcmphs\.32" } }*/
/* { dg-final { scan-assembler "fcmphs\.64" } }*/
