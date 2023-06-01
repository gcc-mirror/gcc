/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef unsigned int v2si  __attribute__((vector_size (8)));

#define force_gp(V1)   asm volatile (""				\
           : "=r"(V1)                                           \
           : "r"(V1)                                            \
           : /* No clobbers */);

/*
** foo:
**	ldr	(x[0-9]+), \[x1\]
**	str	\1, \[x0\]
**	ret
*/

void
foo (v2si *a, v2si *b)
{
  v2si tmp = *b;
  force_gp (tmp);
  *a = tmp;
}

/*
** foo2:
**	ldp	(x[0-9]+), (x[0-9]+), \[x0\]
**	stp	\1, \2, \[x1\]
**	ret
*/
void
foo2 (v2si *a, v2si *b)
{
  v2si t1 = *a;
  v2si t2 = a[1];
  force_gp (t1);
  force_gp (t2);
  *b = t1;
  b[1] = t2;
}
