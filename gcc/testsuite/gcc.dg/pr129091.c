/* PR middle-end/129091 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O0" } */

struct S { long long : 63; __int128 b : 64; };

void
foo (struct S *p)
{
  p->b = 0;
}
