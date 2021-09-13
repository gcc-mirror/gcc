/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

float func32(float a, float b, float c){
  a += b * c;
  return a;
}

double func64(double a, double b, double c){
  a += b * c;
  return a;
}

/* { dg-final { scan-assembler "fmula\.32" } }*/
/* { dg-final { scan-assembler "fmula\.64" } }*/
