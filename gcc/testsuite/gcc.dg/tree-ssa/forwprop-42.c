/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */

typedef unsigned int vec2 __attribute__ ((vector_size (2 * sizeof (unsigned int))));
typedef unsigned int vec1 __attribute__ ((vector_size (sizeof (unsigned int))));

vec1 foo (vec2 x)
{
	return (vec1) x[1];
}

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "forwprop1" } } */
