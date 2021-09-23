/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "" } */

float recip32(float a){
  return 1.0f/a;
}

double recip64(double a){
  return 1.0/a;
}

/* { dg-final { scan-assembler "frecip\.32" } }*/
/* { dg-final { scan-assembler "frecip\.64" } }*/
