/* Use conditional compare */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { arm_thumb1_ok || arm_cortex_m } } */
/* { dg-final { scan-assembler "cmpne" } } */

int f(int i, int j) 
{
  if ( (i == '+') || (j == '-') ) {
    return 1;
  } else {
    return 0;
  }
}
