/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* RV64 only.  */
int foos(short *x, int n){
  return x[n];
}
int fooi(int *x, int n){
  return x[n];
}
int fooll(long long *x, int n){
  return x[n];
}

/* RV64 only.  */
int ufoos(short *x, unsigned int n){
  return x[n];
}
int ufooi(int *x, unsigned int n){
  return x[n];
}
int ufooll(long long *x, unsigned int n){
  return x[n];
}

/* { dg-final { scan-assembler-times "sh1add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh2add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add.uw" 1 } } */
/* { dg-final { scan-assembler-times "sh3add.uw" 1 } } */
/* { dg-final { scan-assembler-times "sh3add.uw" 1 } } */
