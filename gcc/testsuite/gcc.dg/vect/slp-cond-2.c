/* { dg-require-effective-target vect_cond_mixed } */
#include "tree-vect.h"

#define N 32
int d[N], e[N], f[N];
unsigned char k[N];
float a[N], b[N];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N/4; i++)
    {
      k[4*i] = a[4*i] < b[4*i] ? 17 : 0;
      k[4*i+1] = a[4*i+1] < b[4*i+1] ? 17 : 0;
      k[4*i+2] = a[4*i+2] < b[4*i+2] ? 17 : 0;
      k[4*i+3] = a[4*i+3] < b[4*i+3] ? 17 : 0;
    }
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N/2; ++i)
    {
      k[2*i] = a[2*i] < b[2*i] ? 0 : 24;
      k[2*i+1] = a[2*i+1] < b[2*i+1] ? 7 : 4;
    }
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < N/2; ++i)
    {
      k[2*i] = a[2*i] < b[2*i] ? 51 : 12;
      k[2*i+1] = a[2*i+1] > b[2*i+1] ? 51 : 12;
    }
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  int i;
  for (i = 0; i < N/2; ++i)
    {
      int d0 = d[2*i], e0 = e[2*i];
      int d1 = d[2*i+1], e1 = e[2*i+1];
      f[2*i] = a[2*i] >= b[2*i] ? d0 : e0;
      f[2*i+1] = a[2*i+1] >= b[2*i+1] ? d1 : e1;
    }
}

int
main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      switch (i % 9)
	{
	case 0: asm (""); a[i] = - i - 1; b[i] = i + 1; break;
	case 1: a[i] = 0; b[i] = 0; break;
	case 2: a[i] = i + 1; b[i] = - i - 1; break;
	case 3: a[i] = i; b[i] = i + 7; break;
	case 4: a[i] = i; b[i] = i; break;
	case 5: a[i] = i + 16; b[i] = i + 3; break;
	case 6: a[i] = - i - 5; b[i] = - i; break;
	case 7: a[i] = - i; b[i] = - i; break;
	case 8: a[i] = - i; b[i] = - i - 7; break;
	}
      d[i] = i;
      e[i] = 2 * i;
    }

  f1 ();
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 ? 17 : 0))
      abort ();

  f2 ();
  for (i = 0; i < N; i++)
    {
      switch (i % 9)
        {
        case 0:
	case 6:
	  if (k[i] != ((i/9 % 2) == 0 ? 0 : 7))
	    abort ();
	  break;
        case 1:
        case 5:
        case 7:
	  if (k[i] != ((i/9 % 2) == 0 ? 4 : 24))
            abort ();
          break;
        case 2:
        case 4:
        case 8:
	  if (k[i] != ((i/9 % 2) == 0 ? 24 : 4))
            abort ();
          break;
        case 3:
	  if (k[i] != ((i/9 % 2) == 0 ? 7 : 0))
            abort ();
          break;
        }
    }

  f3 ();

  f4 ();
  for (i = 0; i < N; i++)
    if (f[i] != ((i % 3) == 0 ? e[i] : d[i]))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
