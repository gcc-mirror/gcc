/* { dg-do compile } */

#include <stdlib.h>
#include <stdio.h>

typedef struct {
  int *a;
  int *b;
  int *c;
} mystruct;

int main(int argc, char* argv[])
{
  const int N = 1024;
  const int S = 32;
  mystruct *m = (mystruct *) calloc (S, sizeof (*m));
  int i, j;

  for (i = 0; i < S; i++)
    {
      m[i].a = (int *) malloc (N * sizeof (int));
      m[i].b = (int *) malloc (N * sizeof (int));
      m[i].c = (int *) malloc (N * sizeof (int));
    }

  for (j = 0; j < S; j++)
    for (i = 0; i < N; i++)
      {
	m[j].a[i] = 0;
	m[j].b[i] = 0;
	m[j].c[i] = 0;
      }

#pragma acc enter data copyin(m[0:1])

  for (int i = 0; i < 99; i++)
    {
      int j, k;
      for (k = 0; k < S; k++)
#pragma acc parallel loop copy(m[k].a[0:N]) /* { dg-error "expected .\\\). before .\\\.. token" } */
        for (j = 0; j < N; j++)
          m[k].a[j]++;

      for (k = 0; k < S; k++)
#pragma acc parallel loop copy(m[k].b[0:N], m[k].c[5:N-10]) /* { dg-error "expected .\\\). before .\\\.. token" } */
	/* { dg-error ".m. appears more than once in data clauses" "" { target c++ } .-1 } */
	for (j = 0; j < N; j++)
	  {
	    m[k].b[j]++;
	    if (j > 5 && j < N - 5)
	      m[k].c[j]++;
	}
    }

#pragma acc exit data copyout(m[0:1])

  for (j = 0; j < S; j++)
    {
      for (i = 0; i < N; i++)
	{
	  if (m[j].a[i] != 99)
	    abort ();
	  if (m[j].b[i] != 99)
	    abort ();
	  if (i > 5 && i < N-5)
	    {
	      if (m[j].c[i] != 99)
		abort ();
	    }
	  else
	    {
	      if (m[j].c[i] != 0)
		abort ();
	    }
	}

      free (m[j].a);
      free (m[j].b);
      free (m[j].c);
    }
  free (m);

  return 0;
}
