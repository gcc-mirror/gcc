/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

extern int *g;
extern const int *q;

void
f (int i)
{
  int j;

  if (i == 0)
    for (j = 0; j < 10; j++)
       ++*g;
  else
    for (j = 0; j < 10; j++)
       ++*g;

  if (i == 1)
    {
      int i = 10;
      *g = i;
    }
  else
    {
      int i = 10;
      *g = i;
    }

  if (i == 3)
    q = (const int []){1};
  else
    q = (const int []){1};
}
