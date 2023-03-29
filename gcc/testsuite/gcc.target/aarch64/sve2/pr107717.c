/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv8-a+sve2" } */

void foo(int n, char *restrict out, char *restrict in) {
  for (int i=n; i-->0; ) {
    out[i] += in[i];
  }
}
