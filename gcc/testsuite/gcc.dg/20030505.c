/* { dg-do compile { target powerpc-*-eabispe* } } */
/* { dg-options "-W" } */

#define __vector __attribute__((vector_size(8)))

typedef float                   __vector __ev64_fs__;

__ev64_opaque__ *p1;
__ev64_fs__ *p2;
int *x;

extern void f (__ev64_opaque__ *);

int main ()
{
	f (x);	/* { dg-warning "incompatible pointer type" } */
	f (p1);
	f (p2);
	return 0;
}
