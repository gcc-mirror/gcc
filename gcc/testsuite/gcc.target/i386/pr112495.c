/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct { int v; } T1;
typedef struct { T1 v[32]; } T2;

T1 s;
T1 f1() { return s; }

void f2(__seg_gs T2 *p, int n) {
  for (int i = 0; i < n; ++i) p->v[i] = f1();
}
