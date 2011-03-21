/* Test case for PR23625 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ftree-loop-linear" } */

typedef long INT32;
void find_best_colors ()
{
int ic0, ic1, ic2;
INT32 * bptr;
INT32 dist1;
INT32 dist2;
INT32 xx1;
for (ic0 = (1<<(5 -3))-1;ic0 >= 0;ic0--)
{
  for (ic1 = (1<<(6 -3))-1;ic1 >= 0;ic1--)
  {
    dist2 = dist1;
     for (ic2 = (1<<(5 -3))-1;ic2 >= 0;ic2--)
     {
        *bptr = dist2;
        bptr++;
     }
     dist1 += xx1;
  }
}
}

