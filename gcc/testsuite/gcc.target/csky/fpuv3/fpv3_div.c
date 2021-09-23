/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "" } */

float div32(float a, float b){
  return a/b;
}


float div64(double a, double b){
  return a/b;
}

/* { dg-final { scan-assembler "fdiv\.32" } }*/
/* { dg-final { scan-assembler "fdiv\.64" } }*/
