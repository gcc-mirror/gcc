/* { dg-do run { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

/* Program to test PowerPC AltiVec instructions.  */

#include <altivec.h>
#include "altivec_check.h"

extern void abort (void);
#define CHECK_IF(E) if(!(E)) abort()

vector int a1 = (vector int){ 100, 200, 300, 400 };
vector int a2 = (vector int){ 500, 600, 700, 800 };
vector int addi = (vector int){ 600, 800, 1000, 1200 };
vector int avgi = (vector int){ 300, 400, 500, 600 };

vector float f1 = (vector float){ 1.0, 2.0, 3.0, 4.0 };
vector float f2 = (vector float){ 5.0, 6.0, 7.0, 8.0 };
vector float f3;
vector float addf1 = (vector float){ 6.0, 8.0, 10.0, 12.0 };
vector float addf2 = (vector float){ 6.1, 8.1, 10.1, 12.1 };
vector float addf3 = (vector float){ 6.0, 8.0, 9.9, 12.1 };
vector int k;
vector float f, g, h;

int main ()
{

  altivec_check();  /* Exit if AltiVec not available.  */

  k = vec_add (a1, a2);
  CHECK_IF (vec_all_eq (addi, k));
  CHECK_IF (vec_all_ge (addi, k));
  CHECK_IF (vec_all_le (addi, k));
  CHECK_IF (vec_any_eq (addi, k));
  CHECK_IF (vec_any_ge (addi, k));
  CHECK_IF (vec_any_le (addi, k));
  CHECK_IF (!vec_any_ne (addi, k));
  CHECK_IF (!vec_any_lt (addi, k));
  CHECK_IF (!vec_any_gt (addi, k));
  CHECK_IF (!vec_any_ne (addi, k));
  CHECK_IF (!vec_any_lt (addi, k));
  CHECK_IF (!vec_any_gt (addi, k));

  k = vec_avg (a1, a2);
  CHECK_IF (vec_all_eq (k, avgi));

  h = vec_add (f1, f2);
  CHECK_IF (vec_all_eq (h, addf1));
  CHECK_IF (vec_all_ge (h, addf1));
  CHECK_IF (vec_all_le (h, addf1));
  CHECK_IF (vec_any_eq (h, addf1));
  CHECK_IF (vec_any_ge (h, addf1));
  CHECK_IF (vec_any_le (h, addf1));
  CHECK_IF (!vec_any_ne (h, addf1));
  CHECK_IF (!vec_any_lt (h, addf1));
  CHECK_IF (!vec_any_gt (h, addf1));
  CHECK_IF (!vec_any_ne (h, addf1));
  CHECK_IF (!vec_any_lt (h, addf1));
  CHECK_IF (!vec_any_gt (h, addf1));

  CHECK_IF (vec_all_gt (addf2, addf1));
  CHECK_IF (vec_any_gt (addf2, addf1));
  CHECK_IF (vec_all_ge (addf2, addf1));
  CHECK_IF (vec_any_ge (addf2, addf1));
  CHECK_IF (vec_all_ne (addf2, addf1));
  CHECK_IF (vec_any_ne (addf2, addf1));
  CHECK_IF (!vec_all_lt (addf2, addf1));
  CHECK_IF (!vec_any_lt (addf2, addf1));
  CHECK_IF (!vec_all_le (addf2, addf1));
  CHECK_IF (!vec_any_le (addf2, addf1));
  CHECK_IF (!vec_all_eq (addf2, addf1));
  CHECK_IF (!vec_any_eq (addf2, addf1));

  CHECK_IF (vec_any_eq (addf3, addf1));
  CHECK_IF (vec_any_ne (addf3, addf1));
  CHECK_IF (vec_any_lt (addf3, addf1));
  CHECK_IF (vec_any_le (addf3, addf1));
  CHECK_IF (vec_any_gt (addf3, addf1));
  CHECK_IF (vec_any_ge (addf3, addf1));
  CHECK_IF (!vec_all_eq (addf3, addf1));
  CHECK_IF (!vec_all_ne (addf3, addf1));
  CHECK_IF (!vec_all_lt (addf3, addf1));
  CHECK_IF (!vec_all_le (addf3, addf1));
  CHECK_IF (!vec_all_gt (addf3, addf1));
  CHECK_IF (!vec_all_ge (addf3, addf1));

  CHECK_IF (vec_all_numeric (addf3));
  CHECK_IF (vec_all_in (addf1, addf2));

  CHECK_IF (vec_step (vector bool char) == 16);
  CHECK_IF (vec_step (addf3) == 4);

  return 0;
}
