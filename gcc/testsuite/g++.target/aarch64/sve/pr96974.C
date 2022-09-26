/* { dg-do compile } */
/* { dg-options "-Ofast -march=armv8.2-a+sve -fdisable-tree-fre4" } */

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
