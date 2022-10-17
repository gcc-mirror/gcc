/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

#define M 11

struct S
{
  float x;
  float y;
} pS[100];

float a[1000];
float b[1000];

void
foo (int n)
{
  int i, j;

  for (i = 0; i < n; i++)
    {
      pS[i].x = 0;
      pS[i].y = 0;

      for (j = 0; j < M; j++)
        {
          pS[i].x += (a[i]+b[i]);
          pS[i].y += (a[i]-b[i]);
        }
    }
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" } } */
