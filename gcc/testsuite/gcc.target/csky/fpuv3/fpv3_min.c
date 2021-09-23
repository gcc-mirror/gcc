/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

extern float fminf( float x, float y );
float fmin32(float a, float b){
  return fminf(a, b);
}

extern double fmin( double x, double y );
double fmin64(double a, double b){
  return fmin(a, b);
}

/* { dg-final { scan-assembler "fminnm\.32" } }*/
/* { dg-final { scan-assembler "fminnm\.64" } }*/
