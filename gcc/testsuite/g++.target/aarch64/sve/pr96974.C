/* { dg-do compile } */
/* { dg-options "-Ofast -march=armv8.2-a+sve -fdisable-tree-fre4 -fdump-tree-slp-details" } */

float a;
int
b ()
{ return __builtin_lrintf(a); }

struct c {
  float d;
    c() {
      for (int e = 0; e < 9; e++)
	coeffs[e] = d ? b() : 0;
    }
    int coeffs[10];
} f;

/* { dg-final { scan-tree-dump "Not vectorized: Incompatible number of vector subparts between" "slp1" { target lp64 } } } */
