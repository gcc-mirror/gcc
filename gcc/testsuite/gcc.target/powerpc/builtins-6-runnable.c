/* { dg-do run { target { powerpc*-*-* && { lp64 && p8vector_hw } } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */

#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <altivec.h>

#define TRUE 1
#define FALSE 0

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int result_wrong_sc (vector signed char vec_expected,
		     vector signed char vec_actual)
{
  int i;

  for (i = 0; i < 16; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_sc (vector signed char vec_expected,
	       vector signed char vec_actual)
{
  int i;

  printf("expected signed char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual signed char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_uc (vector unsigned char vec_expected,
		     vector unsigned char vec_actual)
{
  int i;

  for (i = 0; i < 16; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_uc (vector unsigned char vec_expected,
	       vector unsigned char vec_actual)
{
  int i;

  printf("expected unsigned char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual unsigned char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_us (vector unsigned short vec_expected,
		     vector unsigned short vec_actual)
{
  int i;

  for (i = 0; i < 8; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_us (vector unsigned short vec_expected,
	       vector unsigned short vec_actual)
{
  int i;

  printf("expected unsigned short data\n");
  for (i = 0; i < 8; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual unsigned short data\n");
  for (i = 0; i < 8; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_ss (vector signed short vec_expected,
		     vector signed short vec_actual)
{
  int i;

  for (i = 0; i < 8; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_ss (vector signed short vec_expected,
	       vector signed short vec_actual)
{
  int i;

  printf("expected signed short data\n");
  for (i = 0; i < 8; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual signed short data\n");
  for (i = 0; i < 8; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_ui (vector unsigned int vec_expected,
		     vector unsigned int vec_actual)
{
  int i;

  for (i = 0; i < 4; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_ui (vector unsigned int vec_expected,
	       vector unsigned int vec_actual)
{
  int i;

  printf("expected unsigned int data\n");
  for (i = 0; i < 4; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual unsigned int data\n");
  for (i = 0; i < 4; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_si (vector signed int vec_expected,
		     vector signed int vec_actual)
{
  int i;

  for (i = 0; i < 4; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_si (vector signed int vec_expected,
	       vector signed int vec_actual)
{
  int i;

  printf("expected signed int data\n");
  for (i = 0; i < 4; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual signed int data\n");
  for (i = 0; i < 4; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}

int result_wrong_ull (vector unsigned long long vec_expected,
		      vector unsigned long long vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_ull (vector unsigned long long vec_expected,
		vector unsigned long long vec_actual)
{
  int i;

  printf("expected unsigned long long data\n");
  for (i = 0; i < 2; i++)
    printf(" %llu,", vec_expected[i]);

  printf("\nactual unsigned long long data\n");
  for (i = 0; i < 2; i++)
    printf(" %llu,", vec_actual[i]);
  printf("\n");
}

int result_wrong_sll (vector signed long long vec_expected,
		      vector signed long long vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_sll (vector signed long long vec_expected,
		vector signed long long vec_actual)
{
  int i;

  printf("expected signed long long data\n");
  for (i = 0; i < 2; i++)
    printf(" %lld,", vec_expected[i]);

  printf("\nactual signed long long data\n");
  for (i = 0; i < 2; i++)
    printf(" %lld,", vec_actual[i]);
  printf("\n");
}

int result_wrong_u128 (vector __uint128_t vec_expected,
		       vector __uint128_t vec_actual)
{
  int i;

    if (vec_expected[0] != vec_actual[0])
      return TRUE;

  return FALSE;
}

void print_u128 (vector __uint128_t vec_expected,
		 vector __uint128_t vec_actual)
{
  printf("expected uint128 data\n");
  printf(" %llu%llu\n", (unsigned long long)(vec_expected[0] >> 64),
	 (unsigned long long)(vec_expected[0] & 0xFFFFFFFFFFFFFFFF));

  printf("\nactual uint128 data\n");
  printf(" %llu%llu\n", (unsigned long long)(vec_actual[0] >> 64),
	 (unsigned long long)(vec_actual[0] & 0xFFFFFFFFFFFFFFFF));
}


int result_wrong_s128 (vector __int128_t vec_expected,
		       vector __int128_t vec_actual)
{
  int i;

    if (vec_expected[0] != vec_actual[0])
      return TRUE;

  return FALSE;
}

void print_s128 (vector __int128 vec_expected,
		 vector __int128 vec_actual)
{
  printf("expected int128 data\n");
  printf(" %lld%llu\n", (signed long long)(vec_expected[0] >> 64),
	 (unsigned long long)(vec_expected[0] & 0xFFFFFFFFFFFFFFFF));

  printf("\nactual int128 data\n");
  printf(" %lld%llu\n", (signed long long)(vec_actual[0] >> 64),
	 (unsigned long long)(vec_actual[0] & 0xFFFFFFFFFFFFFFFF));
}

int result_wrong_d (vector double vec_expected,
		    vector double vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_d (vector double vec_expected,
	      vector double vec_actual)
{
  int i;

  printf("expected double data\n");
  for (i = 0; i < 2; i++)
    printf(" %f,", vec_expected[i]);

  printf("\nactual double data\n");
  for (i = 0; i < 2; i++)
    printf(" %f,", vec_actual[i]);
  printf("\n");
}

int result_wrong_f (vector float vec_expected,
		    vector float vec_actual)
{
  int i;

  for (i = 0; i < 4; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

void print_f (vector float vec_expected,
	      vector float vec_actual)
{
  int i;

  printf("expected float data\n");
  for (i = 0; i < 4; i++)
    printf(" %f,", vec_expected[i]);

  printf("\nactual float data\n");
  for (i = 0; i < 4; i++)
    printf(" %f,", vec_actual[i]);
  printf("\n");
}

int main() {
   int i, j;
   size_t len;
   vector signed char store_data_sc;
   vector unsigned char store_data_uc;
   vector signed int store_data_si;
   vector unsigned int store_data_ui;
   vector __int128_t store_data_s128;
   vector __uint128_t store_data_u128;
   vector signed long long int store_data_sll;
   vector unsigned long long int store_data_ull;
   vector signed short store_data_ss;
   vector unsigned short store_data_us;
   vector double store_data_d;
   vector float store_data_f;

   signed char *address_sc;
   unsigned char *address_uc;
   signed int *address_si;
   unsigned int *address_ui;
   __int128_t *address_s128;
   __uint128_t *address_u128;
   signed long long int *address_sll;
   unsigned long long int *address_ull;
   signed short int *address_ss;
   unsigned short int *address_us;
   double *address_d;
   float *address_f;

   vector unsigned char *datap;

   vector unsigned char vec_uc_expected1, vec_uc_result1;
   vector signed char vec_sc_expected1, vec_sc_result1;
   vector signed int vec_si_expected1, vec_si_result1;
   vector unsigned int vec_ui_expected1, vec_ui_result1;
   vector __int128_t vec_s128_expected1, vec_s128_result1;
   vector __uint128_t vec_u128_expected1, vec_u128_result1;
   vector signed long long int vec_sll_expected1, vec_sll_result1;
   vector unsigned long long int vec_ull_expected1, vec_ull_result1;
   vector signed short int vec_ss_expected1, vec_ss_result1;
   vector unsigned short int vec_us_expected1, vec_us_result1;
   vector double vec_d_expected1, vec_d_result1;
   vector float vec_f_expected1, vec_f_result1;

   signed long long disp;

   /* VEC_XST */
   disp = 0;
   vec_sc_expected1 = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					    1, 2, 3, 4, 5, 6, 7, 8 };
   store_data_sc = (vector signed char){  -7, -6, -5, -4, -3, -2, -1, 0,
					  1, 2, 3, 4, 5, 6, 7, 8 };

   for (i=0; i<16; i++)
     vec_sc_result1[i] = 0;

   address_sc = &vec_sc_result1[0];

   vec_xst (store_data_sc, disp, address_sc);

   if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, sc disp = 0, result does not match expected result\n");
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
     }

   disp = 2;
   vec_sc_expected1 = (vector signed char){  0, 0, -7, -6, -5, -4, -3, -2,
					     -1, 0, 1, 2, 3, 4, 5, 6 };
   store_data_sc = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					 1, 2, 3, 4, 5, 6, 7, 8 };

   for (i=0; i<16; i++)
     vec_sc_result1[i] = 0;

   address_sc = &vec_sc_result1[0];

   vec_xst (store_data_sc, disp, address_sc);

   if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, sc disp = 2, result does not match expected result\n");
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_uc_expected1 = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					      8, 9, 10, 11, 12, 13, 14, 15 };
   store_data_uc = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					   8, 9, 10, 11, 12, 13, 14, 15 };

   for (i=0; i<16; i++)
     vec_uc_result1[i] = 0;

   address_uc = &vec_uc_result1[0];

   vec_xst (store_data_uc, disp, address_uc);

   if (result_wrong_uc (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, uc disp = 0, result does not match expected result\n");
       print_uc (vec_uc_expected1, vec_uc_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_ss_expected1 = (vector signed short int){ -4, -3, -2, -1, 0, 1, 2, 3 };
   store_data_ss = (vector signed short int){ -4, -3, -2, -1, 0, 1, 2, 3 };

   for (i=0; i<8; i++)
     vec_ss_result1[i] = 0;

   address_ss = &vec_ss_result1[0];

   vec_xst (store_data_ss, disp, address_ss);

   if (result_wrong_ss (vec_ss_expected1, vec_ss_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, ss disp = 0, result does not match expected result\n");
       print_ss (vec_ss_expected1, vec_ss_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_us_expected1 = (vector unsigned short int){ 0, 1, 2, 3, 4, 5, 6, 7 };
   store_data_us = (vector unsigned short int){ 0, 1, 2, 3, 4, 5, 6, 7 };

   for (i=0; i<8; i++)
     vec_us_result1[i] = 0;

   address_us = &vec_us_result1[0];

   vec_xst (store_data_us, disp, address_us);

   if (result_wrong_us (vec_us_expected1, vec_us_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, us disp = 0, result does not match expected result\n");
       print_us (vec_us_expected1, vec_us_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_si_expected1 = (vector signed int){ -2, -1, 0, 1 };
   store_data_si = (vector signed int){ -2, -1, 0, 1 };

   for (i=0; i<4; i++)
     vec_si_result1[i] = 0;

   address_si = &vec_si_result1[0];

   vec_xst (store_data_si, disp, address_si);

   if (result_wrong_si (vec_si_expected1, vec_si_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, si disp = 0, result does not match expected result\n");
       print_si (vec_si_expected1, vec_si_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_ui_expected1 = (vector unsigned int){ -2, -1, 0, 1 };
   store_data_ui = (vector unsigned int){ -2, -1, 0, 1 };

   for (i=0; i<4; i++)
     vec_ui_result1[i] = 0;

   address_ui = &vec_ui_result1[0];

   vec_xst (store_data_ui, disp, address_ui);

   if (result_wrong_ui (vec_ui_expected1, vec_ui_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, ui disp = 0, result does not match expected result\n");
       print_ui (vec_ui_expected1, vec_ui_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_sll_expected1 = (vector signed long long){ -1, 0 };
   store_data_sll = (vector signed long long ){ -1, 0 };

   for (i=0; i<2; i++)
     vec_sll_result1[i] = 0;

   address_sll = (signed long long *)(&vec_sll_result1[0]);

   vec_xst (store_data_sll, disp, address_sll);

   if (result_wrong_sll (vec_sll_expected1, vec_sll_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, sll disp = 0, result does not match expected result\n");
       print_sll (vec_sll_expected1, vec_sll_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_ull_expected1 = (vector unsigned long long){ 0, 1 };
   store_data_ull = (vector unsigned long long){  0, 1 };

   for (i=0; i<2; i++)
     vec_ull_result1[i] = 0;

   address_ull = (unsigned long long int *)(&vec_ull_result1[0]);

   vec_xst (store_data_ull, disp, address_ull);

   if (result_wrong_ull (vec_ull_expected1, vec_ull_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, ull disp = 0, result does not match expected result\n");
       print_ull (vec_ull_expected1, vec_ull_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_s128_expected1 = (vector __int128_t){ 12345 };
   store_data_s128 = (vector __int128_t){  12345 };

   vec_s128_result1[0] = 0;

   address_s128 = (__int128_t *)(&vec_s128_result1[0]);

   vec_xst (store_data_s128, disp, address_s128);

   if (result_wrong_s128 (vec_s128_expected1, vec_s128_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, s128 disp = 0, result does not match expected result\n");
       print_s128 (vec_s128_expected1, vec_s128_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_u128_expected1 = (vector __uint128_t){ 12345 };
   store_data_u128 = (vector __uint128_t){  12345 };

   vec_u128_result1[0] = 0;

   address_u128 = (__int128_t *)(&vec_u128_result1[0]);

   vec_xst (store_data_u128, disp, address_u128);

   if (result_wrong_u128 (vec_u128_expected1, vec_u128_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, u128 disp = 0, result does not match expected result\n");
       print_u128 (vec_u128_expected1, vec_u128_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_d_expected1 = (vector double){ 0, 1 };
   store_data_d = (vector double){  0, 1 };

   for (i=0; i<2; i++)
     vec_d_result1[i] = 0;

   address_d = (double *)(&vec_d_result1[0]);

   vec_xst (store_data_d, disp, address_d);

   if (result_wrong_d (vec_d_expected1, vec_d_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, double disp = 0, result does not match expected result\n");
       print_d (vec_d_expected1, vec_d_result1);
#else
       abort();
#endif
     }

   disp = 0;
   vec_f_expected1 = (vector float){ 0, 1 };
   store_data_f = (vector float){  0, 1 };

   for (i=0; i<4; i++)
     vec_f_result1[i] = 0;

   address_f = (float *)(&vec_f_result1[0]);

   vec_xst (store_data_f, disp, address_f);

   if (result_wrong_f (vec_f_expected1, vec_f_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst, float disp = 0, result does not match expected result\n");
       print_f (vec_f_expected1, vec_f_result1);
#else
       abort();
#endif
     }

   /* VEC_XST_BE, these always load in BE order regardless of
      machine endianess.  */
   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_sc_expected1 = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					    1, 2, 3, 4, 5, 6, 7, 8 };
#else
   vec_sc_expected1 = (vector signed char){ 8, 7, 6, 5, 4, 3, 2, 1,
					    0, -1, -2, -3, -4, -5, -6, -7 };
#endif
   store_data_sc = (vector signed char){  -7, -6, -5, -4, -3, -2, -1, 0,
					  1, 2, 3, 4, 5, 6, 7, 8 };

   for (i=0; i<16; i++)
     vec_sc_result1[i] = 0;

   address_sc = &vec_sc_result1[0];

   vec_xst_be (store_data_sc, disp, address_sc);

   if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, sc disp = 0, result does not match expected result\n");
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
     }

   disp = 2;
#ifdef __BIG_ENDIAN__
   vec_sc_expected1 = (vector signed char){  0, 0, -7, -6, -5, -4, -3, -2,
					     -1, 0, 1, 2, 3, 4, 5, 6 };
#else
   vec_sc_expected1 = (vector signed char){  0, 0, 8, 7, 6, 5, 4, 3,
					     2, 1, 0, -1, -2, -3, -4, -5 };
#endif
   store_data_sc = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					 1, 2, 3, 4, 5, 6, 7, 8 };

   for (i=0; i<16; i++)
     vec_sc_result1[i] = 0;

   address_sc = &vec_sc_result1[0];

   vec_xst_be (store_data_sc, disp, address_sc);

   if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, sc disp = 2, result does not match expected result\n");
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					      8, 9, 10, 11, 12, 13, 14, 15 };
#else
   vec_uc_expected1 = (vector unsigned char){ 15, 14, 13, 12, 11, 10, 9, 8,
					      7, 6, 5, 4, 3, 2, 1 };
#endif
   store_data_uc = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					   8, 9, 10, 11, 12, 13, 14, 15 };

   for (i=0; i<16; i++)
     vec_uc_result1[i] = 0;

   address_uc = &vec_uc_result1[0];

   vec_xst_be (store_data_uc, disp, address_uc);

   if (result_wrong_uc (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, uc disp = 0, result does not match expected result\n");
       print_uc (vec_uc_expected1, vec_uc_result1);
#else
       abort();
#endif
     }

   disp = 8;
#ifdef __BIG_ENDIAN__
   vec_si_expected1 = (vector signed int){  0, 0, -8, -7 };
#else
   vec_si_expected1 = (vector signed int){  0, 0, -5, -6 };
#endif
   store_data_si = (vector signed int){ -8, -7, -6, -5 };

   for (i=0; i<4; i++)
     vec_si_result1[i] = 0;

   address_si = &vec_si_result1[0];

   vec_xst_be (store_data_si, disp, address_si);

   if (result_wrong_si (vec_si_expected1, vec_si_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, si disp = %d, result does not match expected result\n", disp);
       print_si (vec_si_expected1, vec_si_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_ui_expected1 = (vector unsigned int){ 0, 1, 2, 3 };
#else
   vec_ui_expected1 = (vector unsigned int){ 3, 2, 1, 0 };
#endif
   store_data_ui = (vector unsigned int){ 0, 1, 2, 3 };

   for (i=0; i<4; i++)
     vec_ui_result1[i] = 0;

   address_ui = &vec_ui_result1[0];

   vec_xst_be (store_data_ui, disp, address_ui);

   if (result_wrong_ui (vec_ui_expected1, vec_ui_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, ui disp = 0, result does not match expected result\n");
       print_ui (vec_ui_expected1, vec_ui_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_ss_expected1 = (vector signed short int){ -4, -3, -2, -1, 0, 1, 2, 3 };
#else
   vec_ss_expected1 = (vector signed short int){ 3, 2, 1, 0, -1, -2, -3, -4 };
#endif
   store_data_ss = (vector signed short int){ -4, -3, -2, -1, 0, 1, 2, 3 };

   for (i=0; i<8; i++)
     vec_ss_result1[i] = 0;

   address_ss = &vec_ss_result1[0];

   vec_xst_be (store_data_ss, disp, address_ss);

   if (result_wrong_ss (vec_ss_expected1, vec_ss_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, ss disp = 0, result does not match expected result\n");
       print_ss (vec_ss_expected1, vec_ss_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_us_expected1 = (vector unsigned short int){ 0, 1, 2, 3, 4, 5, 6, 7 };
#else
   vec_us_expected1 = (vector unsigned short int){ 7, 6, 5, 4, 3, 2, 1, 0 };
#endif
   store_data_us = (vector unsigned short int){ 0, 1, 2, 3, 4, 5, 6, 7 };

   for (i=0; i<8; i++)
     vec_us_result1[i] = 0;

   address_us = &vec_us_result1[0];

   vec_xst_be (store_data_us, disp, address_us);

   if (result_wrong_us (vec_us_expected1, vec_us_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, us disp = 0, result does not match expected result\n");
       print_us (vec_us_expected1, vec_us_result1);
#else
       abort();
#endif
     }

#if 0
   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_si_expected1 = (vector signed int){ -2, -1, 0, 1 };
#else
   vec_si_expected1 = (vector signed int){ 1, 0, -1, -2 };
#endif
   store_data_si = (vector signed int){ -2, -1, 0, 1 };

   for (i=0; i<4; i++)
     vec_si_result1[i] = 0;

   address_si = &vec_si_result1[0];

   vec_xst_be (store_data_si, disp, address_si);
   if (result_wrong_si (vec_si_expected1, vec_si_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, si disp = 0, result does not match expected result\n");
       print_si (vec_si_expected1, vec_si_result1);
#else
       abort();
#endif
     }
#endif

#if 0
   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_ui_expected1 = (vector unsigned int){ -2, -1, 0, 1 };
#else
   vec_ui_expected1 = (vector unsigned int){ 1, 0, -1, -2 };
#endif
   store_data_ui = (vector unsigned int){ -2, -1, 0, 1 };

   for (i=0; i<4; i++)
     vec_ui_result1[i] = 0;

   address_ui = &vec_ui_result1[0];

   vec_xst_be (store_data_ui, disp, address_ui);

   if (result_wrong_ui (vec_ui_expected1, vec_ui_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, ui disp = 0, result does not match expected result\n");
       print_ui (vec_ui_expected1, vec_ui_result1);
#else
       abort();
#endif
     }
#endif
   
   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_sll_expected1 = (vector signed long long){ -1, 0 };
#else
   vec_sll_expected1 = (vector signed long long){ 0, -1 };
#endif
   store_data_sll = (vector signed long long ){ -1, 0 };

   for (i=0; i<2; i++)
     vec_sll_result1[i] = 0;

   address_sll = (signed long long *)(&vec_sll_result1[0]);

   vec_xst_be (store_data_sll, disp, address_sll);

   if (result_wrong_sll (vec_sll_expected1, vec_sll_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, sll disp = 0, result does not match expected result\n");
       print_sll (vec_sll_expected1, vec_sll_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_ull_expected1 = (vector unsigned long long){ 0, 1234567890123456 };
#else
   vec_ull_expected1 = (vector unsigned long long){1234567890123456, 0 };
#endif   
   store_data_ull = (vector unsigned long long){  0, 1234567890123456 };

   for (i=0; i<2; i++)
     vec_ull_result1[i] = 0;

   address_ull = (unsigned long long int *)(&vec_ull_result1[0]);

   vec_xst_be (store_data_ull, disp, address_ull);

   if (result_wrong_ull (vec_ull_expected1, vec_ull_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, ull disp = 0, result does not match expected result\n");
       print_ull (vec_ull_expected1, vec_ull_result1);
#else
       abort();
#endif
     }

   disp = 0;

#ifdef __BIG_ENDIAN__
   vec_s128_expected1 = (vector __int128_t){ (__uint128_t)12345678911121314 };
#else
   vec_s128_expected1 = (vector __int128_t){ (__uint128_t)12345678911121314 };
#endif
   store_data_s128 = (vector __int128_t)(__uint128_t){  12345678911121314 };

   vec_s128_result1[0] = 0;

   address_s128 = (__int128_t *)(&vec_s128_result1[0]);

   vec_xst_be (store_data_s128, disp, address_s128);

   if (result_wrong_s128 (vec_s128_expected1, vec_s128_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, s128 disp = 0, result does not match expected result\n");
       print_s128 (vec_s128_expected1, vec_s128_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_u128_expected1 = (vector __uint128_t){ (__uint128_t)1234567891112131415 };
#else
   vec_u128_expected1 = (vector __uint128_t){ (__uint128_t)1234567891112131415 };
#endif
   store_data_u128 = (vector __uint128_t){ (__uint128_t)1234567891112131415 };

   vec_u128_result1[0] = 0;

   address_u128 = (__int128_t *)(&vec_u128_result1[0]);

   vec_xst_be (store_data_u128, disp, address_u128);

   if (result_wrong_u128 (vec_u128_expected1, vec_u128_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, u128 disp = 0, result does not match expected result\n");
       print_u128 (vec_u128_expected1, vec_u128_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_d_expected1 = (vector double){ 0.0, 1.1 };
#else
   vec_d_expected1 = (vector double){ 1.1, 0.0 };
#endif
   store_data_d = (vector double){  0.0, 1.1 };

   for (i=0; i<2; i++)
     vec_d_result1[i] = 0;

   address_d = (double *)(&vec_d_result1[0]);

   vec_xst_be (store_data_d, disp, address_d);

   if (result_wrong_d (vec_d_expected1, vec_d_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, double disp = 0, result does not match expected result\n");
       print_d (vec_d_expected1, vec_d_result1);
#else
       abort();
#endif
     }

   disp = 0;
#ifdef __BIG_ENDIAN__
   vec_f_expected1 = (vector float){ 0.0, 1.2, 2.3, 3.4 };
#else
   vec_f_expected1 = (vector float){ 3.4, 2.3, 1.2, 0.0 };
#endif
   store_data_f = (vector float){ 0.0, 1.2, 2.3, 3.4 };

   for (i=0; i<4; i++)
     vec_f_result1[i] = 0;

   address_f = (float *)(&vec_f_result1[0]);

   vec_xst_be (store_data_f, disp, address_f);

   if (result_wrong_f (vec_f_expected1, vec_f_result1))
     {
#ifdef DEBUG
       printf("Error: vec_xst_be, float disp = 0, result does not match expected result\n");
       print_f (vec_f_expected1, vec_f_result1);
#else
       abort();
#endif
     }
}
