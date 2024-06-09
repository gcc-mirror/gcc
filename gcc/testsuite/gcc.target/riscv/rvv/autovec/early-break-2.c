/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -fdump-tree-vect-details" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define N 1728

unsigned vect_a[N];
unsigned vect_b[N];

/*
** test:
** ...
** vmsltu\.vv\s+v[0-9]+\s*,v[0-9]+,\s*v[0-9]+
** vcpop\.m\s+[atx][0-9]+\s*,v[0-9]+
** ...
*/
unsigned test (unsigned limit, int n)
{
  unsigned ret = 0;

  for (int i = 0; i < n; i++)
    {
      vect_b[i] = limit + i;

      if (vect_a[i] > limit)
	{
	  ret = vect_b[i];
	  return ret;
	}

      vect_a[i] = limit;
    }

  return ret;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" } } */
