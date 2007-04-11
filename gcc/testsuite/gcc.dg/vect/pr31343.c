/* { dg-do compile } */

#define N 16
 
struct
{
  unsigned int x;
  unsigned int y;
} pS [100];

void
main1 ()
{
  int i, j;
  unsigned int ub[N] =
    { 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45 };
  unsigned int uc[N] =
    { 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
  unsigned int udiffx, udiffy;

  for (i = 0; i < N; i++)
    {
      pS[i].x = 0;
      pS[i].y = 0;
      for (j = 0; j < N; j++)
      {
        udiffx = (ub[j] - uc[j]);
        udiffy = (ub[j] - uc[j]);
        pS[i].x = udiffx;
        pS[i].y = udiffy;
      }
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
