/* Verify a statement in the GCC Manual that GCC allows the use of a
   typedef name as a vector type specifier.  */

/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec" } */

typedef unsigned int ui;
typedef signed char sc;
__vector ui vui;
__vector sc vsc;
