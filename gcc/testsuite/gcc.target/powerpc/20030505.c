/* { dg-do compile } */
/* { dg-options "-W -mcpu=8540 -mspe -mabi=spe -mfloat-gprs=single" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } { "*" } { "" } } */

#define __vector __attribute__((vector_size(8)))

typedef float                   __vector __ev64_fs__;

__ev64_opaque__ *p1;
__ev64_fs__ *p2;
int *x;

extern void f (__ev64_opaque__ *); /* { dg-message "expected.*but argument is of type" } */

int main ()
{
	f (x);	/* { dg-warning "incompatible pointer type" } */
	f (p1);
	f (p2);
	return 0;
}
