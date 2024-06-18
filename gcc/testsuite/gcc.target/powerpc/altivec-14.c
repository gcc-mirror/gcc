/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-options "-maltivec -mno-vsx" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool long vbl;		/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
vector signed long vsl;		/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
vector unsigned long vul;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
vector bool long *pvbl;		/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
vector signed long *pvsl;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
vector unsigned long *pvul;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */

void fvbl (vector bool long v) { }	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
void fvsl (vector signed long v) { }	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
void fvul (vector unsigned long v) { }	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */

int main ()
{
  vector bool long lvbl;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
  vector signed long lvsl;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
  vector unsigned long lvul;	/* { dg-warning "use of .long. in AltiVec types is deprecated; use .int." } */
  return 0;
}
