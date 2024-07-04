/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

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
