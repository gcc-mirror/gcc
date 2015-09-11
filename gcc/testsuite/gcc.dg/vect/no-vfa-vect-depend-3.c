/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

int ia[N + 1];
int ib[N + 1];

/* Vectorizable. Dependence distance -1.  */
__attribute__((noinline)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      ia[i + 1] = 1;
      ib[i] = ia[i];
    }
}

/* Not vectorizable due to data dependence: dependence distance 1.  */
__attribute__((noinline)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      ia[i] = 1;
      ib[i] = ia[i + 1];
    }
}

/* Not vectorizable due to data dependence: dependence distance 1.  */
__attribute__((noinline)) void
f3 (void)
{
  int i;
  for (i = N - 1; i >= 0; i--)
    {
      ia[i + 1] = 1;
      ib[i] = ia[i];
    }
}

/* Vectorizable. Dependence distance -1.  */
__attribute__((noinline)) void
f4 (void)
{
  int i;
  for (i = N - 1; i >= 0; i--)
    {
      ia[i] = 1;
      ib[i] = ia[i + 1];
    }
}

/* Vectorizable. Dependence distance -1.  */
__attribute__((noinline)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      ia[i + 1] = 1;
      ia[i] = 2;
    }
}

/* Not vectorizable due to data dependence: dependence distance 1.  */
__attribute__((noinline)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      ia[i] = 1;
      ia[i + 1] = 2;
    }
}

/* Not vectorizable due to data dependence: dependence distance 1.  */
__attribute__((noinline)) void
f7 (void)
{
  int i;
  for (i = N - 1; i >= 0; i--)
    {
      ia[i + 1] = 1;
      ia[i] = 2;
    }
}

/* Vectorizable. Dependence distance -1.  */
__attribute__((noinline)) void
f8 (void)
{
  int i;
  for (i = N - 1; i >= 0; i--)
    {
      ia[i] = 1;
      ia[i + 1] = 2;
    }
}

__attribute__ ((noinline)) int
main1 (void)
{
  int i, j;

  for (j = 0; j < 8; j++)
    {
      for (i = 0; i <= N; i++)
	{
	  ia[i] = i + 3;
	  ib[i] = i + N + 3;
	  asm ("");
	}

      switch (j)
	{
	case 0: f1 (); break;
	case 1: f2 (); break;
	case 2: f3 (); break;
	case 3: f4 (); break;
	case 4: f5 (); break;
	case 5: f6 (); break;
	case 6: f7 (); break;
	case 7: f8 (); break;
	}

      for (i = 0; i <= N; i++)
	{
	  int ea = i + 3;
	  int eb = i + N + 3;
	  switch (j)
	    {
	    case 0:
	      if (i) ea = 1;
	      if (i == 0) eb = 3;
	      else if (i != N) eb = 1;
	      break;
	    case 1:
	      if (i != N) ea = 1;
	      if (i != N) eb = i + 4;
	      break;
	    case 2:
	      if (i) ea = 1;
	      if (i != N) eb = i + 3;
	      break;
	    case 3:
	      if (i != N) ea = 1;
	      if (i < N - 1) eb = 1;
	      else if (i == N - 1) eb = 67;
	      break;
	    case 4:
	      ea = 1 + (i != N);
	      break;
	    case 5:
	      ea = 2 - (i != N);
	      break;
	    case 6:
	      ea = 1 + (i == 0);
	      break;
	    case 7:
	      ea = 2 - (i == 0);
	      break;
	    }
	  if (ia[i] != ea || ib[i] != eb)
	    abort ();
	}
    }

  return 0;
}

int main ()
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" {xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "dependence distance negative" 4 "vect"  } } */
