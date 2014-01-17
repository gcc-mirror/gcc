/* { dg-do compile } */   
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */
/* { dg-options "-mabi=altivec" { target { { powerpc*-*-linux* } && ilp32 } } } */
/* { dg-options "-msse2" { target { i?86-*-* x86_64-*-* } } } */
/* Ignore warning on some powerpc-ibm-aix configurations. */
/* { dg-prune-output "non-standard ABI extension" } */

/* Test for C_MAYBE_CONST are folded correctly when 
   expanding an expression to vector.  */

int 			f(void);
unsigned int 		g(void);
unsigned int 		h;

typedef unsigned int vec __attribute__((vector_size(16)));

vec i;


vec fv1(void) { return i + (h ? f() : g()); }
vec fv2(void) { return (h ? f() : g()) + i; }
