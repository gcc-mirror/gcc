/* { dg-do compile } */
#include <altivec.h>
extern void referencen001(int *p1, int *p2, int *p3, int *p4, int *p5, int *p6, int *p7, vector float *p8);

extern int gn0011;

extern int gn0012;

extern int gn0013;

extern int gn0014;

extern int gn0015;

extern int gn0016;

extern int gn0017;

extern vector float gn0018;

void testn001(void)
{
  int a1;
  int a2;
  int a3;
  int a4;
  int a5;
  int a6;
  int a7;
  vector float a8;

  (a1 = -53786696, a2 = -1840132710, a3 = -2130504990, a4 = 1429848501, a5 = 1139248605, a6 = 428762253, a7 = -1581480596, a8 = ((vector float) {1.66e+09, -1.83e+09, -6.79e+08, 1.58e+09}));

  referencen001(&a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8);
}
