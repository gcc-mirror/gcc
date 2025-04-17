/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=256 --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */
/* { dg-final { check-function-bodies "**" "" ""} } */

#define N 512
#define START 1
#define END 505
 
int x[N] __attribute__((aligned(32)));

/*
** foo:
**	...
**	ld1w	z[0-9]+.s, p[0-9]+/z, \[x[0-9], x[0-9], lsl 2\]
**	cmple	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	...
*/

int __attribute__((noipa))
foo (void)
{
  int z = 0;
  for (unsigned int i = START; i < END; ++i)
    {
      z++;
      if (x[i] > 0)
        continue;
    
      return z;
    }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "pfa_iv_offset" "vect" } } */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */

