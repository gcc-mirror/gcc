/* PR tree-optimization/101189  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

static int a, b;
int main() {
  int d = 0, e, f = 5;
  if (a)
    f = 0;
  for (; f < 4; f++)
    ;
  e = f ^ -f;
  e && d;
  if (!e)
    e || b;
  return 0;
}
