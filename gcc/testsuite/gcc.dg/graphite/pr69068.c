/* { dg-do compile } */
/* { dg-options "-O1 -fgraphite-identity" } */

int qo;
int zh[2];

void
td (void)
{
  int ly, en;
  for (ly = 0; ly < 2; ++ly)
    for (en = 0; en < 2; ++en)
      zh[en] = ((qo == 0) || (((qo * 2) != 0))) ? 1 : -1;
}
