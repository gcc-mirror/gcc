/* PR tree-optimization/98766.  */

/* { dg-do compile } */
/* { dg-options "-O3 --param=avoid-fma-max-bits=8 " } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64*-*-* } } */

extern int a[];
void c(short *d) {
  for (int e = 0; e < 9; e++)
    a[e] = d[e] * 2;
}

