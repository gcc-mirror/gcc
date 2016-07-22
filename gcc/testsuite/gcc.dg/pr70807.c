/* PR middle-end/70807 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int INT;
int a, b, c, d, e, f;
void fn1() {
  INT g;
  if (d && a)
    ;
  else if (e && b)
    ;
  else if (!a && !b && c)
    ;
  else if (b && d || a && e)
    a = 0;
  f = g || d;
}
