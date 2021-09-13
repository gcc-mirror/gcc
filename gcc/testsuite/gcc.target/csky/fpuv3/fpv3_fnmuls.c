/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2" } */

float fnmula32(float a, float b, float c){
  return -a + b * c;
}

double fnmula64(double a, double b, double c){
  return -a + b * c;
}

/* { dg-final { scan-assembler "ffnmuls\.32" } }*/
/* { dg-final { scan-assembler "ffnmuls\.64" } }*/
