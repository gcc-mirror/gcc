/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Program to test PowerPC AltiVec instructions.  */

#include <altivec.h>
#include <stdio.h>

extern void abort (void);
#define CHECK_IF(E) if(!(E)) abort()

vector char sca = {0,4,8,1,5,9,2,6,10,3,7,11,15,12,14,13};
vector char scb = {6,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};
vector char sc_expected = {3,4,8,2,3,9,2,6,10,3,7,11,15,12,14,13};
vector char scz;
vector unsigned char uca = {0,4,8,1,5,9,2,6,10,3,7,11,15,12,14,13};
vector unsigned char ucb = {6,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};
vector unsigned char uc_expected = {3,4,8,2,3,9,2,6,10,3,7,11,15,12,14,13};
vector unsigned char ucz;

vector short int ssia = {9, 16, 25, 36};
vector short int ssib = {-8, -27, -64, -125};
vector short int ssiz;

vector short unsigned int ssua = {9, 16, 25, 36};
vector short unsigned int ssub = {8, 27, 64, 125};
vector short unsigned int ssuz;

vector unsigned int uia = {22, 13, 24, 39};
vector unsigned int uib = {8, 7, 4, 15};
vector unsigned int ui_expected = {15, 10, 14, 27};
vector unsigned int uiz;

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

int i;

int main ()
{

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

  ssiz = vec_avg (ssia, ssib);
  ssuz = vec_avg (ssua, ssub);
  k = vec_avg (a1, a2);
  scz = vec_avg (sca, scb);

  for (i=0; i< 16; i++)
    if (scz[i] != sc_expected[i])
      abort ();
  
  ucz = vec_avg (uca, ucb);

  for (i=0; i<16; i++)
    if (ucz[i] != uc_expected[i])
      abort ();
  
  uiz = vec_avg (uia, uib);

  for (i=0; i< 4; i++)
    if (uiz[i] != ui_expected[i])
      abort ();
  
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
