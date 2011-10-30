/* { dg-do compile } */   
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */

/* Test for C_MAYBE_CONST are folded correctly when 
   expanding an expression to vector.  */

int 			f(void);
unsigned int 		g(void);
unsigned int 		h;

typedef unsigned int vec __attribute__((vector_size(16)));

vec i;


vec fv1(void) { return i + (h ? f() : g()); }
vec fv2(void) { return (h ? f() : g()) + i; }
