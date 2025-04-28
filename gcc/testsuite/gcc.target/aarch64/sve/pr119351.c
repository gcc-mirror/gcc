/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=256 --param aarch64-autovec-preference=2 -fdump-tree-vect-details" } */
/* { dg-final { check-function-bodies "**" "" ""} } */

#define N 512
#define START 0
#define END 505
 
int x[N] __attribute__((aligned(32)));

/*
** foo:
**	...
**	orr	p[0-9]+.b, p[0-9]+/z, p[0-9]+.b, p[0-9]+.b
**	...
*/

int __attribute__((noipa))
foo (void)
{
  int z = 0;
  for (unsigned short i = START; i < END; ++i)
    {
      z++;
      if (x[i] > 0)
        continue;
    
      return z;
    }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

