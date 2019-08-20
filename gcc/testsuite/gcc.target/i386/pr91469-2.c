/* { dg-do compile } */
/* { dg-options "-Os --param scev-max-expr-size=0 -mavx512vnni -funroll-all-loops" } */

int a, b, c, d;
int *e;
void fn1()
{
  b = c > 0 ? c : 0;
  d += e[b];
  a = d > 0 ? d : 0;
}
