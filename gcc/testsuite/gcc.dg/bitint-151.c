/* PR middle-end/128865 */
/* { dg-do compile } */
/* { dg-options "-std=c2y -O2" } */

enum E : signed _BitInt(256) { E90 = 90 };

int
foo (enum E x)
{
  return x > E90 || x < 0;
}
