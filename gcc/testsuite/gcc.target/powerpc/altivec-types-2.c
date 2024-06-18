/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-options "-maltivec -mno-vsx" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_altivec } */

/* These should get warnings for 32-bit code.  */

__vector long vl;			/* { dg-warning "deprecated" } */
__vector unsigned long vul;		/* { dg-warning "deprecated" } */
__vector signed long vsl;		/* { dg-warning "deprecated" } */
__vector __bool long int vbli;		/* { dg-warning "deprecated" } */
__vector long int vli;			/* { dg-warning "deprecated" } */
__vector unsigned long int vuli;	/* { dg-warning "deprecated" } */
__vector signed long int vsli;		/* { dg-warning "deprecated" } */
