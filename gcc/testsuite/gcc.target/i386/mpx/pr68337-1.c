/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

#include "mpx-check.h"

#define N 2

extern void abort ();

static int
mpx_test (int argc, const char **argv)
{
  char ** src = (char **)malloc (sizeof (char *) * N);
  char ** dst = (char **)malloc (sizeof (char *) * N);
  int i;

  for (i = 0; i < N; i++)
    src[i] = __bnd_set_ptr_bounds (argv[0] + i, i + 1);

  __builtin_memcpy(dst, src, sizeof (char *) * N);

  for (i = 0; i < N; i++)
    {
      char *p = dst[i];
      if (p != argv[0] + i
	  || __bnd_get_ptr_lbound (p) != p
	  || __bnd_get_ptr_ubound (p) != p + i)
	abort ();
    }

  return 0;
}
