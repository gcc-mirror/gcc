/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#define B bool
#define P pixel
#define I int
#define BI bool int
#define PI pixel int

vector B int i;
vector P int j;
vector B I k;
vector P I l;
vector BI m;
vector PI n;
