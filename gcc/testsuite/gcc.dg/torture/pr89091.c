/* PR middle-end/89091 */
/* { dg-do compile { target int128 } } */

struct S { unsigned __int128 s : 65; };

int
foo (struct S *x, int y)
{
  return y && x->s;
}
