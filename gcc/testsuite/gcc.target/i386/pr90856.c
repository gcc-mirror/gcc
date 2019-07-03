/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct { int v; } S1;
typedef struct { S1 s1[32]; } S2;

S1 clearS1() { S1 s; s.v = 1; return s; }

void
clearS2(__seg_gs S2 *p, int n)
{
  for (int i = 0; i < n; ++i)
    p->s1[i] = clearS1();
}
