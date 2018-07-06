/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-skip-if "using short-double" { avr-*-* } } */

int f(double aaa, double bbb){
  int xa = __builtin_isunordered(aaa, aaa);
  int xb = __builtin_isunordered(bbb, bbb);
  return xa | xb;
}

int g(double aaa, double bbb){
  int xa = __builtin_isunordered(aaa, bbb);
  int xb = __builtin_isunordered(bbb, bbb);
  return xa | xb;
}

int h(double ccc, float ddd){
  int xc = __builtin_isunordered(ccc, ccc);
  int xd = __builtin_isunordered(ddd, ddd);
  return xc | xd;
}

/* { dg-final { scan-tree-dump-not "aaa\[^\n\r\]* unord aaa" "optimized" } } */
/* { dg-final { scan-tree-dump-not "bbb\[^\n\r\]* unord bbb" "optimized" } } */
/* { dg-final { scan-tree-dump-times "aaa\[^\n\r\]* unord bbb" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "ccc\[^\n\r\]* unord ddd" "optimized" } } */
