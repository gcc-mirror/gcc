/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O3 -fprofile-generate" } */

struct {
    int prefix;
    int dir_idx;
} *a;
int b;
void fn1() {
    int *c, *d;
    for (; b; b++)
      if (d[b]) {
	  c[b] = d[b];
	  a[0].dir_idx = 0;
      }
}
