/* { dg-do compile } */
/* { dg-options "-std=c23" } */

// adapted from PR c/11428.

struct s { int m : 1; char (*y)[]; } s;

int
foo (void *q)
{
	struct s { int m : 1; char (*y)[1]; } t;
	typeof(1 ? &s : &t) p = q;
	return !p->m;
}

