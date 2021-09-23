/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "" } */

float fnez32(float a, float b, float c){
  if(a != 0.0f)
    return b;
  else
    return c;
}

double fnez64(double a, double b, double c){
  if( a != 0.0)
    return b;
  else
    return c;
}

/* { dg-final { scan-assembler "fcmpnez\.32" } }*/
/* { dg-final { scan-assembler "fcmpnez\.64" } }*/

