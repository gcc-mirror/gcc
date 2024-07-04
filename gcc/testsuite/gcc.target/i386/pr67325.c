/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "(?:sar|shr)" } } */

int f(long*l){
  return *l>>32;
}
