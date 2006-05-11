/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Program to test PowerPC AltiVec instructions.  */

#include <altivec.h>
#include "altivec_check.h"

extern void abort (void);

vector int a1 = { 100, 200, 300, 400 };
vector int a2 = { 500, 600, 700, 800 };
vector int addi = { 600, 800, 1000, 1200 };
vector int avgi = { 300, 400, 500, 600 };

vector float f1 = { 1.0, 2.0, 3.0, 4.0 };  
vector float f2 = { 5.0, 6.0, 7.0, 8.0 };
vector float f3;
vector float addf = { 6.0, 8.0, 10.0, 12.0 };

vector int k;
vector float f, g, h;

int main ()
{
  altivec_check();  /* Exits if AltiVec not supported */

  k = vec_add (a1, a2);
  if (!vec_all_eq (addi, k))
    abort ();

  k = vec_avg (a1, a2);
  if (!vec_all_eq (k, avgi))
    abort ();

  h = vec_add (f1, f2);
  if (!vec_all_eq (h, addf))
    abort ();

  return 0;
}
