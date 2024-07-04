/* { dg-do compile } */
/* { dg-options "-maltivec -fdump-tree-gimple" } */
/* { dg-require-effective-target powerpc_altivec } */

/* PR 72747: Test that cascaded definition is happening for constant vectors. */

#include <altivec.h>

int main (int argc, char *argv[])
{
	__vector int v1,v2;
	v1 = v2 = vec_splats ((int) 42);
	return 0;
}
/* { dg-final { scan-tree-dump-times " v2 = { 42, 42, 42, 42 }" 1 "gimple" } } */

