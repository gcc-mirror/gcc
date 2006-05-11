/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* These should get warnings for 32-bit code.  */

__vector long vl;			/* { dg-warning "deprecated" "" } */
__vector unsigned long vul;		/* { dg-warning "deprecated" "" } */
__vector signed long vsl;		/* { dg-warning "deprecated" "" } */
__vector __bool long int vbli;		/* { dg-warning "deprecated" "" } */
__vector long int vli;			/* { dg-warning "deprecated" "" } */
__vector unsigned long int vuli;	/* { dg-warning "deprecated" "" } */
__vector signed long int vsli;		/* { dg-warning "deprecated" "" } */
