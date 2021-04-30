/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2 -mfpu=fpv3" } */

__fp16 func16(__fp16 a, __fp16 b, __fp16 c){
  a -= b * c;
  return a;
}

float func32(float a, float b, float c){
  a -= b * c;
  return a;
}


double func64(double a, double b, double c){
  a -= b * c;
  return a;
}

/* { dg-final { scan-assembler "ffmuls\.16" } }*/
/* { dg-final { scan-assembler "ffmuls\.32" } }*/
/* { dg-final { scan-assembler "ffmuls\.64" } }*/
