/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

extern float fmaxf( float x, float y );
float fmax32(float a, float b){
  return fmaxf(a, b);
}

extern double fmax( double x, double y );
double fmax64(double a, double b){
  return fmax(a, b);
}

/* { dg-final { scan-assembler "fmaxnm\.32" } }*/
/* { dg-final { scan-assembler "fmaxnm\.64" } }*/
