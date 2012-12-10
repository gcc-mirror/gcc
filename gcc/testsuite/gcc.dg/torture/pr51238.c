/* { dg-do compile } */
/* { dg-options "-ftree-loop-distribution" } */

extern int ia[];
extern int ib[];
int i;

void
foo (int l)
{
  while (l--)
    {
      i = ia[l];
      ia[l] = ib[l] = 0;
    }
}
