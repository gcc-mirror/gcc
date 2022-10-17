/* PR rtl-optimization/104544 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fcompare-debug" } */

int m, n;
__int128 q;

void
bar (unsigned __int128 x, int y)
{
  if (x)
    q += y;
}

void
foo (void)
{
  bar (!!q - 1, (m += m ? m : 1) < n);
}
