/* PR c/82167 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long long a;
a b;
float
c ()
{
  float d = b > 0;
  return d;
}
