/* Verify a statement in the GCC Manual that GCC allows the use of a
   typedef name as a vector type specifier.  */

/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec -mabi=altivec" } */

typedef unsigned int ui;
typedef signed char sc;
__vector ui vui;
__vector sc vsc;
