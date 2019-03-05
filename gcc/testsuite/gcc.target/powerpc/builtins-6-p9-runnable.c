/* { dg-do run { target { powerpc*-*-* &&  p9vector_hw } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <altivec.h> // vector

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

int result_wrong_uc (vector unsigned char vec_expected,
		     vector unsigned char vec_actual)
{
  int i;

  for (i = 0; i < 16; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
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

int result_wrong_ss (vector signed short vec_expected,
		     vector signed short vec_actual)
{
  int i;

  for (i = 0; i < 8; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
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

int result_wrong_ull (vector unsigned long long vec_expected,
		      vector unsigned long long vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

int result_wrong_sll (vector signed long long vec_expected,
		      vector signed long long vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    {
      if (vec_expected[i] != vec_actual[i])
        return TRUE;
    }

  return FALSE;
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

int result_wrong_u128 (vector __uint128_t vec_expected,
		       vector __uint128_t vec_actual)
{
  int i;

  if (vec_expected[0] != vec_actual[0])
    return TRUE;

  return FALSE;
}

int result_wrong_s128 (vector __int128_t vec_expected,
		       vector __int128_t vec_actual)
{
  int i;

  if (vec_expected[0] != vec_actual[0])
    return TRUE;

  return FALSE;
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

int result_wrong_f (vector float vec_expected,
		    vector float vec_actual)
{
  int i;

  for (i = 0; i < 4; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

#ifdef DEBUG
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

void print_uc (vector unsigned char vec_expected,
	       vector unsigned char vec_actual)
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
#endif

int main() {
  int i, j;
  size_t len;
  signed char data_c[100];
  vector signed char input_c;
  unsigned char data_uc[100];
  vector unsigned char input_uc;

  signed short int data_ssi[100];
  vector signed short int input_ssi;
  unsigned short int data_usi[100];
  vector unsigned short int input_usi;

  signed int data_si[100];
  vector signed int input_si;
  unsigned int data_ui[100];
  vector unsigned int input_ui;

  signed long long data_sll[100];
  vector signed long long  input_sll;
  unsigned long long data_ull[100];
  vector unsigned long long int input_ull;

  float data_f[100];
  vector float input_f;
  double data_d[100];
  vector double input_d;
  __uint128_t data_u128[100];
  vector __uint128_t input_u128;
  __int128_t data_128[100];
  vector __int128_t input_128;

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

  /* VEC_XST_LEN, ISA 3.0 */
  len = 16;
  vec_sc_expected1 = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					   1, 2, 3, 4, 5, 6, 7, 8 };
  store_data_sc = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					1, 2, 3, 4, 5, 6, 7, 8 };
  vec_sc_result1 = (vector signed char){ 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0 };

  address_sc = &vec_sc_result1[0];

  vec_xst_len (store_data_sc, address_sc, len);

  if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, sc result does not match expected result\n", len);
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
    }

  len = 12;
  vec_sc_expected1 = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					   1, 2, 3, 4, 0, 0, 0, 0 };
  store_data_sc = (vector signed char){ -7, -6, -5, -4, -3, -2, -1, 0,
					1, 2, 3, 4, 5, 6, 7, 8 };
  vec_sc_result1 = (vector signed char){ 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0 };

  address_sc = &vec_sc_result1[0];

  vec_xst_len (store_data_sc, address_sc, len);

  if (result_wrong_sc (vec_sc_expected1, vec_sc_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, sc result does not match expected result\n",
	      len);
       print_sc (vec_sc_expected1, vec_sc_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_uc_expected1 = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					     8, 9, 10, 11, 12, 13, 14, 15 };

  store_data_uc = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					  8, 9, 10, 11, 12, 13, 14, 15 };

  vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };

  address_uc = &vec_uc_result1[0];

  vec_xst_len (store_data_uc, address_uc, len);

  if (result_wrong_uc (vec_uc_expected1, vec_uc_result1))
    {
#ifdef DEBUG
      printf("Error: vec_xst_len, len = %d, uc result does not match expected result\n",
	     len);
      print_uc (vec_uc_expected1, vec_uc_result1);
#else
      abort();
#endif
    }

  len = 4;
  vec_uc_expected1 = (vector unsigned char){ 0, 1, 2, 3, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0 };
  store_data_uc = (vector unsigned char){ 0, 1, 2, 3, 4, 5, 6, 7,
					  8, 9, 10, 11, 12, 13, 14, 15 };
  vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };

  address_uc = &vec_uc_result1[0];

  vec_xst_len (store_data_uc, address_uc, len);

  if (result_wrong_uc (vec_uc_expected1, vec_uc_result1))
    {
#ifdef DEBUG
      printf("Error: vec_xst_len, len = %d, uc result does not match expected result\n",
	      len);
      print_uc (vec_uc_expected1, vec_uc_result1);
#else
      abort();
#endif
    }

  len = 16;
  vec_ss_expected1 = (vector signed short int){ 10, 20, 30, 40,
						50, 60, 70, 80 };
  store_data_ss = (vector signed short int){ 10, 20, 30, 40,
					     50, 60, 70, 80 };
  vec_ss_result1 = (vector signed short int){ 0, 0, 0, 0, 0, 0, 0, 0};

  address_ss = &vec_ss_result1[0];

  vec_xst_len (store_data_ss, address_ss, len);

  if (result_wrong_ss (vec_ss_expected1, vec_ss_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, ss result does not match expected result\n",
	      len);
       print_ss (vec_ss_expected1, vec_ss_result1);
#else
       abort();
#endif
    }

  len = 14;
  vec_ss_expected1 = (vector signed short int){ 10, 20, 30, 40,
						50, 60, 70, 0 };
  store_data_ss = (vector signed short int){ 10, 20, 30, 40,
					     50, 60, 70, 80 };
  vec_ss_result1 = (vector signed short int){ 0, 0, 0, 0, 0, 0, 0, 0};

  address_ss = &vec_ss_result1[0];

  vec_xst_len (store_data_ss, address_ss, len);

  if (result_wrong_ss (vec_ss_expected1, vec_ss_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, ss result does not match expected result\n",
	      len);
       print_ss (vec_ss_expected1, vec_ss_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_us_expected1 = (vector unsigned short int){ 10, 20, 30, 40,
						  50, 60, 70, 80 };
  store_data_us = (vector unsigned short int){ 10, 20, 30, 40,
					       50, 60, 70, 80 };
  vec_us_result1 = (vector unsigned short int){ 0, 0, 0, 0, 0, 0, 0, 0};

  address_us = &vec_us_result1[0];

  vec_xst_len (store_data_us, address_us, len);

  if (result_wrong_us (vec_us_expected1, vec_us_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, us result does not match expected result\n",
	      len);
       print_us (vec_us_expected1, vec_us_result1);
#else
       abort();
#endif
    }

  len = 2;
  vec_us_expected1 = (vector unsigned short int){ 10, 0, 0, 0,
						  0, 0, 0, 0 };
  store_data_us = (vector unsigned short int){ 10, 20, 30, 40,
					       50, 60, 70, 80 };
  vec_us_result1 = (vector unsigned short int){ 0, 0, 0, 0, 0, 0, 0, 0};

  address_us = &vec_us_result1[0];

  vec_xst_len (store_data_us, address_us, len);

  if (result_wrong_us (vec_us_expected1, vec_us_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, us result does not match expected result\n",
	      len);
       print_us (vec_us_expected1, vec_us_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_si_expected1 = (vector signed int){ -30, -10, 0, 10 };
  store_data_si = (vector signed int){ -30, -10, 0, 10 };
  vec_si_result1 = (vector signed int){ 0, 0, 0, 0};

  address_si = &vec_si_result1[0];

  vec_xst_len (store_data_si, address_si, len);

  if (result_wrong_si (vec_si_expected1, vec_si_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, si result does not match expected result\n",
	      len);
       print_si (vec_si_expected1, vec_si_result1);
#else
       abort();
#endif
    }

  len = 8;
  vec_si_expected1 = (vector signed int){ -30, -10, 0, 0 };
  store_data_si = (vector signed int){ -30, -10, 0, 10 };
  vec_si_result1 = (vector signed int){ 0, 0, 0, 0};

  address_si = &vec_si_result1[0];

  vec_xst_len (store_data_si, address_si, len);

  if (result_wrong_si (vec_si_expected1, vec_si_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, si result does not match expected result\n",
	      len);
       print_si (vec_si_expected1, vec_si_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_sll_expected1 = (vector signed long long int){ -3000, 10000 };
  store_data_sll = (vector signed long long int){ -3000, 10000 };
  vec_sll_result1 = (vector signed long long int){ 0, 0};

  address_sll = (signed long long *)&vec_sll_result1[0];

  vec_xst_len (store_data_sll, address_sll, len);

  if (result_wrong_sll (vec_sll_expected1, vec_sll_result1) == TRUE)
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, sll result does not match expected result\n",
	      len);
       print_sll (vec_sll_expected1, vec_sll_result1);
#else
       abort();
#endif
    }

  len = 8;
  vec_sll_expected1 = (vector signed long long int){ -3000, 0 };
  store_data_sll = (vector signed long long int){ -3000, 10000 };
  vec_sll_result1 = (vector signed long long int){ 0, 0};

  address_sll = (signed long long *)&vec_sll_result1[0];

  vec_xst_len (store_data_sll, address_sll, len);

  if (result_wrong_sll (vec_sll_expected1, vec_sll_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, sll result does not match expected result\n",
	      len);
       print_sll (vec_sll_expected1, vec_sll_result1);
#else
      abort();
#endif
    }

  len = 16;
  vec_ull_expected1 = (vector unsigned long long int){ 50000, 120000 };
  store_data_ull = (vector unsigned long long int){ 50000, 120000 };
  vec_ull_result1 = (vector unsigned long long int){ 0, 0};

  address_ull = (unsigned long long *) &vec_ull_result1[0];

  vec_xst_len (store_data_ull, address_ull, len);

  if (result_wrong_ull (vec_ull_expected1, vec_ull_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, ull result does not match expected result\n",
	      len);
       print_ull (vec_ull_expected1, vec_ull_result1);
#else
       abort();
#endif
    }

  len = 8;
  vec_ull_expected1 = (vector unsigned long long int){ 50000, 0 };
  store_data_ull = (vector unsigned long long int){ 50000, 120000 };
  vec_ull_result1 = (vector unsigned long long int){ 0, 0};

  address_ull = (unsigned long long *) &vec_ull_result1[0];

  vec_xst_len (store_data_ull, address_ull, len);

  if (result_wrong_ull (vec_ull_expected1, vec_ull_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, ull result does not match expected result\n",
	      len);
       print_ull (vec_ull_expected1, vec_ull_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_s128_expected1 = (vector __int128_t){ 12345 };
  store_data_s128 = (vector __int128_t){ 12345 };

  vec_s128_result1[0] = 0;

  address_s128 = (__int128_t *)(&vec_s128_result1[0]);

  vec_xst_len (store_data_s128, address_s128, len);

  if (result_wrong_s128 (vec_s128_expected1, vec_s128_result1))
#ifdef DEBUG
    {
       printf("Error: vec_xst_len, len = %d, s128, result does not match expected result\n",
	      len);
       print_s128 (vec_s128_expected1, vec_s128_result1);
    }
#else
    abort();
#endif

  len = 16;
  vec_u128_expected1 = (vector __uint128_t){ 12345 };
  store_data_u128 = (vector __uint128_t){ 12345 };

  vec_u128_result1[0] = 0;

  address_u128 = (__int128_t *)(&vec_u128_result1[0]);

  vec_xst_len (store_data_u128, address_u128, len);

  if (result_wrong_u128 (vec_u128_expected1, vec_u128_result1))
    {
#ifdef DEBUG
      printf("Error: vec_xst_len, len = %d, u128, result does not match expected result\n", len);
      print_u128 (vec_u128_expected1, vec_u128_result1);
#else
      abort();
#endif
    }

  len = 16;
  vec_d_expected1 = (vector double){ 1., 2. };
  store_data_d = (vector double){ 1., 2. };
  vec_d_result1 = (vector double){ 0., 0. };

  address_d = (double *)(&vec_d_result1[0]);

  vec_xst_len (store_data_d, address_d, len);

  if (result_wrong_d (vec_d_expected1, vec_d_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, double, result does not match expected result\n",
	      len);
       print_d (vec_d_expected1, vec_d_result1);
#else
       abort();
#endif
    }

  len = 8;
  vec_d_expected1 = (vector double){ 1., 0. };
  store_data_d = (vector double){ 1., 2. };
  vec_d_result1 = (vector double){ 0., 0. };

  address_d = (double *)(&vec_d_result1[0]);

  vec_xst_len (store_data_d, address_d, len);

  if (result_wrong_d (vec_d_expected1, vec_d_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, double, result does not match expected result\n",
	      len);
       print_d (vec_d_expected1, vec_d_result1);
#else
       abort();
#endif
    }

  len = 16;
  vec_f_expected1 = (vector float){ 1., 2., 3., 4. };
  store_data_f = (vector float){ 1., 2., 3., 4. };
  vec_f_result1 = (vector float){ 0., 0., 0., 0. };

  address_f = (float *)(&vec_f_result1[0]);

  vec_xst_len (store_data_f, address_f, len);

  if (result_wrong_f (vec_f_expected1, vec_f_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, float, result does not match expected result\n",
	      len);
       print_f (vec_f_expected1, vec_f_result1);
#else
       abort();
#endif
    }

  len = 4;
  vec_f_expected1 = (vector float){ 1., 0., 0., 0. };
  store_data_f = (vector float){ 1., 2., 3., 4. };
  vec_f_result1 = (vector float){ 0., 0., 0., 0. };

  address_f = (float *)(&vec_f_result1[0]);

  vec_xst_len (store_data_f, address_f, len);

  if (result_wrong_f (vec_f_expected1, vec_f_result1))
    {
#ifdef DEBUG
       printf("Error: vec_xst_len, len = %d, float, result does not match expected result\n",
	      len);
       print_f (vec_f_expected1, vec_f_result1);
#else
       abort();
#endif
    }

  /* vec_xl_len() tests */
  for (i = 0; i < 100; i++)
    {
      data_c[i] = i;
      data_uc[i] = i+1;
      data_ssi[i] = i+10;
      data_usi[i] = i+11;
      data_si[i] = i+100;
      data_ui[i] = i+101;
      data_sll[i] = i+1000;
      data_ull[i] = i+1001;
      data_f[i] = i+100000.0;
      data_d[i] = i+1000000.0;
      data_128[i] = i + 12800000;
      data_u128[i] = i + 12800001;
    }

  len = 16;
  vec_sc_expected1 = (vector signed char){0, 1, 2, 3, 4, 5, 6, 7,
					  8, 9, 10, 11, 12, 13, 14, 15};
  vec_sc_result1 = vec_xl_len (data_c, len);

  for (i = 0; i < 16; i++)
    {
      if (vec_sc_result1[i] != vec_sc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_sc_result1[%d] = %d; vec_sc_expected1[%d] = %d\n",
	       len, i,  vec_sc_result1[i], i, vec_sc_expected1[i]);
#else
	abort ();
#endif
    }

  len = 12;

  vec_sc_expected1 = (vector signed char){0, 1, 2, 3, 4, 5, 6, 7,
					  8, 9, 10, 11, 0, 0, 0, 0};
  vec_sc_result1 = vec_xl_len (data_c, len);

  for (i = 0; i < 16; i++)
    {
      if (vec_sc_result1[i] != vec_sc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_sc_result1[%d] = %d; vec_sc_expected1[%d] = %d\n",
	       len, i,  vec_sc_result1[i], i, vec_sc_expected1[i]);
#else
        abort ();
#endif
    }

  len = 16;
  vec_uc_expected1 = (vector unsigned char){1, 2, 3, 4, 5, 6, 7, 8, 9,
					    10, 11, 12, 13, 14, 15, 16};
  vec_uc_result1 = vec_xl_len (data_uc, len);

  for (i = 0; i < 16; i++)
    {
      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       len, i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif
    }

  len = 12;
  vec_uc_expected1 = (vector unsigned char){1, 2, 3, 4, 5, 6, 7, 8, 9,
					    10, 11, 12, 0, 0, 0, 0};
  vec_uc_result1 = vec_xl_len (data_uc, len);

  for (i = 0; i < 16; i++)
    {
      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       len, i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ss_expected1 = (vector signed short){10, 11, 12, 13, 14, 15, 16, 17};

  vec_ss_result1 = vec_xl_len (data_ssi, len);

  for (i = 0; i < 8; i++)
    {
      if (vec_ss_result1[i] != vec_ss_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ss_result1[%d] = %d; vec_ss_expected1[%d] = %d\n",
	       len, i,  vec_ss_result1[i], i, vec_ss_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_ss_expected1 = (vector signed short){10, 11, 12, 13, 0, 0, 0, 0};

  vec_ss_result1 = vec_xl_len (data_ssi, len);

  for (i = 0; i < 8; i++)
    {
      if (vec_ss_result1[i] != vec_ss_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ss_result1[%d] = %d; vec_ss_expected1[%d] = %d\n",
	       len, i,  vec_ss_result1[i], i, vec_ss_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_us_expected1 = (vector unsigned short){11, 12, 13, 14, 15, 16, 17, 18};
  vec_us_result1 = vec_xl_len (data_usi, len);

  for (i = 0; i < 8; i++)
    {
      if (vec_us_result1[i] != vec_us_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_us_result1[%d] = %d; vec_us_expected1[%d] = %d\n",
	       len, i,  vec_us_result1[i], i, vec_us_expected1[i]);
#else
	abort ();
#endif

    }

  len = 8;
  vec_us_expected1 = (vector unsigned short){11, 12, 13, 14, 0, 0, 0, 0};
  vec_us_result1 = vec_xl_len (data_usi, len);

  for (i = 0; i < 8; i++)
    {
      if (vec_us_result1[i] != vec_us_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_us_result1[%d] = %d; vec_us_expected1[%d] = %d\n",
	       len, i,  vec_us_result1[i], i, vec_us_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_si_result1 = vec_xl_len (data_si, len);
  vec_si_expected1 = (vector int){100, 101, 102, 103};

  for (i = 0; i < 4; i++)
    {
      if (vec_si_result1[i] != vec_si_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_si_result1[%d] = %d; vec_si_expected1[%d] = %d\n",
	       len, i,  vec_si_result1[i], i, vec_si_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_si_result1 = vec_xl_len (data_si, len);
  vec_si_expected1 = (vector int){100, 101, 0, 0};

  for (i = 0; i < 4; i++)
    {
      if (vec_si_result1[i] != vec_si_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_si_result1[%d] = %d; vec_si_expected1[%d] = %d\n",
	       len, i,  vec_si_result1[i], i, vec_si_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ui_result1 = vec_xl_len (data_ui, len);
  vec_ui_expected1 = (vector unsigned int){101, 102, 103, 104};

  for (i = 0; i < 4; i++)
    {
      if (vec_ui_result1[i] != vec_ui_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ui_result1[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       len, i,  vec_ui_result1[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_ui_result1 = vec_xl_len (data_ui, len);
  vec_ui_expected1 = (vector unsigned int){101, 102, 0, 0};

  for (i = 0; i < 4; i++)
    {
      if (vec_ui_result1[i] != vec_ui_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ui_result1[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       len, i,  vec_ui_result1[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_sll_result1 = vec_xl_len (data_sll, len);
  vec_sll_expected1 = (vector signed long long){1000, 1001};

  for (i = 0; i < 2; i++)
    {
      if (vec_sll_result1[i] != vec_sll_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_sll_result1[%d] = %lld; vec_sll_expected1[%d] = %lld\n",
	       len, i,  vec_sll_result1[i], i, vec_sll_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_sll_result1 = vec_xl_len (data_sll, len);
  vec_sll_expected1 = (vector signed long long){1000, 0};

  for (i = 0; i < 2; i++)
    {
      if (vec_sll_result1[i] != vec_sll_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_sll_result1[%d] = %lld; vec_sll_expected1[%d] = %lld\n",
	       len, i,  vec_sll_result1[i], i, vec_sll_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ull_result1 = vec_xl_len (data_ull, len);
  vec_ull_expected1 = (vector unsigned long long){1001, 1002};

  for (i = 0; i < 2; i++)
    {
      if (vec_ull_result1[i] != vec_ull_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ull_result1[%d] = %lld; vec_ull_expected1[%d] = %lld\n",
	       len, i,  vec_ull_result1[i], i, vec_ull_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_ull_result1 = vec_xl_len (data_ull, len);
  vec_ull_expected1 = (vector unsigned long long){1001, 0};

  for (i = 0; i < 2; i++)
    {
      if (vec_ull_result1[i] != vec_ull_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_ull_result1[%d] = %lld; vec_ull_expected1[%d] = %lld\n",
	       len, i,  vec_ull_result1[i], i, vec_ull_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_f_result1 = vec_xl_len (data_f, len);
  vec_f_expected1 = (vector float){100000.0, 100001.0, 100002.0, 100003.0};

  for (i = 0; i < 4; i++)
    {
      if (vec_f_result1[i] != vec_f_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_f_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_f_result1[i], i, vec_f_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_f_result1 = vec_xl_len (data_f, len);
  vec_f_expected1 = (vector float){100000.0, 100001.0, 0.0, 0.0};

  for (i = 0; i < 4; i++)
    {
      if (vec_f_result1[i] != vec_f_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_f_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_f_result1[i], i, vec_f_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_d_result1 = vec_xl_len (data_d, len);
  vec_d_expected1 = (vector double){1000000.0, 1000001.0};

  for (i = 0; i < 2; i++)
    {
      if (vec_d_result1[i] != vec_d_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_d_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_d_result1[i], i, vec_d_expected1[i]);
#else
	abort ();
#endif
    }

  len = 8;
  vec_d_result1 = vec_xl_len (data_d, len);
  vec_d_expected1 = (vector double){1000000.0, 0.0};

  for (i = 0; i < 2; i++)
    {
      if (vec_d_result1[i] != vec_d_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_len(), len = %d, vec_d_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_d_result1[i], i, vec_d_expected1[i]);
#else
	abort ();
#endif
    }

  vec_s128_expected1 = (vector __int128_t){12800000};
  vec_s128_result1 = vec_xl_len (data_128, len);

  if (vec_s128_expected1[0] != vec_s128_result1[0])
    {
#ifdef DEBUG
       printf("Error: vec_xl_len(), len = %d, vec_s128_result1[0] = %lld %llu; ",
	      len, vec_s128_result1[0] >> 64,
	      vec_s128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
       printf("vec_s128_expected1[0] = %lld %llu\n",
	      vec_s128_expected1[0] >> 64,
	      vec_s128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
#else
       abort ();
#endif
    }

  vec_u128_result1 = vec_xl_len (data_u128, len);
  vec_u128_expected1 = (vector __uint128_t){12800001};
  if (vec_u128_expected1[0] != vec_u128_result1[0])
#ifdef DEBUG
    {
       printf("Error: vec_xl_len(), len = %d, vec_u128_result1[0] = %lld; ",
	      len, vec_u128_result1[0] >> 64,
	      vec_u128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
       printf("vec_u128_expected1[0] = %lld\n",
	      vec_u128_expected1[0] >> 64,
	      vec_u128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
    }
#else
    abort ();
#endif

    /* Tests to ensure the vec_xl_len() is properly optimized with a pre
       and post data initialization is done.  */

  len = 16;

  vec_sc_expected1 = (vector signed char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   0, 0, 0, 0, 0, 0, 0, 0 };

  input_c = (vector signed char){ 1, 2, 3, 4, 5, 6, 7, 8,
				  0, 0, 0, 0, 0, 0, 0, 0 };
  vec_sc_result1 = vec_xl_len (&input_c[0], len);
  input_c = (vector signed char){ 9, 10, 11, 12, 13, 14, 15, 16,
				  17, 18, 19, 20, 21, 22, 23, 24 };
  
  for (i = 0; i < 16; i++)
    {
      if (vec_sc_result1[i] != vec_sc_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_sc_result1[%d] = %d; vec_sc_expected1[%d] = %d\n",
	       len, i,  vec_sc_result1[i], i, vec_sc_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_uc_expected1 = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8, 9,
					     10, 11, 12, 13, 14, 15, 16 };
  input_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8, 9,
				   10, 11, 12, 13, 14, 15, 16 };
  vec_uc_result1 = vec_xl_len (&input_uc[0], len);
  input_uc = (vector unsigned char){ 10, 11, 12, 13, 14, 15, 16,
				     17, 18, 19, 20, 21, 22, 23, 24 };

  for (i = 0; i < 16; i++)
    {
      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       len, i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ss_expected1 = (vector signed short){ 10, 11, 12, 13, 14, 15, 16, 17 };
  input_ssi = (vector signed short){ 10, 11, 12, 13, 14, 15, 16, 17 };
  vec_ss_result1 = vec_xl_len (&input_ssi[0], len);
  input_ssi = (vector signed short){ 14, 15, 16, 17, 18, 19, 20, 21 };

  for (i = 0; i < 8; i++)
    {
      if (vec_ss_result1[i] != vec_ss_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_ss_result1[%d] = %d; vec_ss_expected1[%d] = %d\n",
	       len, i,  vec_ss_result1[i], i, vec_ss_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_us_expected1 = (vector unsigned short){ 11, 12, 13, 14, 15, 16, 17, 18 };
  input_usi = (vector unsigned short){ 11, 12, 13, 14, 15, 16, 17, 18 };
  vec_us_result1 = vec_xl_len (&input_usi[0], len);
  input_usi = (vector unsigned short){ 15, 16, 17, 18, 19, 20, 21, 22 };

  for (i = 0; i < 8; i++)
    {
      if (vec_us_result1[i] != vec_us_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_us_result1[%d] = %d; vec_us_expected1[%d] = %d\n",
	       len, i,  vec_us_result1[i], i, vec_us_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_si_expected1 = (vector int){ 100, 101, 102, 103 };
  input_si = (vector int){ 100, 101, 102, 103 };
  vec_si_result1 = vec_xl_len (&input_si[0], len);
  input_si = (vector int){ 102, 103, 104, 105 };

  for (i = 0; i < 4; i++)
    {
      if (vec_si_result1[i] != vec_si_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_si_result1[%d] = %d; vec_si_expected1[%d] = %d\n",
	       len, i,  vec_si_result1[i], i, vec_si_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ui_expected1 = (vector unsigned int){101, 102, 103, 104};
  input_ui = (vector unsigned int){101, 102, 103, 104};
  vec_ui_result1 = vec_xl_len (&input_ui[0], len);
  input_ui = (vector unsigned int){ 103, 104, 105, 106 };

  for (i = 0; i < 4; i++)
    {
      if (vec_ui_result1[i] != vec_ui_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_ui_result1[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       len, i,  vec_ui_result1[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif
    }

#if 1  
  len = 16;
  vec_sll_expected1 = (vector signed long long){1000, 1001};
  input_sll = (vector signed long long ){1000, 1001};
  vec_sll_result1 = vec_xl_len ((signed long long int *)&input_sll[0], len);
  input_sll = (vector signed long long){1001, 1002};

  for (i = 0; i < 2; i++)
    {
      if (vec_sll_result1[i] != vec_sll_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_sll_result1[%d] = %lld; vec_sll_expected1[%d] = %lld\n",
	       len, i,  vec_sll_result1[i], i, vec_sll_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_ull_expected1 = (vector unsigned long long int){1001, 1002};
  input_ull = (vector unsigned long long int){1001, 1002};
  vec_ull_result1 = vec_xl_len ((unsigned long long int *)&input_ull[0], len);
  input_ull = (vector unsigned long long int){1002, 1003};

  for (i = 0; i < 2; i++)
    {
      if (vec_ull_result1[i] != vec_ull_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_ull_result1[%d] = %lld; vec_ull_expected1[%d] = %lld\n",
	       len, i,  vec_ull_result1[i], i, vec_ull_expected1[i]);
#else
	abort ();
#endif
    }
#endif

  
  len = 16;
  vec_f_expected1 = (vector float){100000.0, 100001.0, 100002.0, 100003.0};
  input_f = (vector float){100000.0, 100001.0, 100002.0, 100003.0};
  vec_f_result1 = vec_xl_len (&input_f[0], len);
  input_f = (vector float){100001.0, 100002.0, 100003.0, 100004.0};

  for (i = 0; i < 4; i++)
    {
      if (vec_f_result1[i] != vec_f_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_f_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_f_result1[i], i, vec_f_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_d_expected1 = (vector double){1000000.0, 1000001.0};
  input_d = (vector double){1000000.0, 1000001.0};
  vec_d_result1 = vec_xl_len (&input_d[0], len);
  input_d = (vector double){1000001.0, 1000002.0};

  for (i = 0; i < 2; i++)
    {
      if (vec_d_result1[i] != vec_d_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_d_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       len, i,  vec_d_result1[i], i, vec_d_expected1[i]);
#else
	abort ();
#endif
    }

  len = 16;
  vec_s128_expected1 = (vector __int128_t){12800000};
  input_128 = (vector __int128_t){12800000};
  vec_s128_result1 = vec_xl_len (&input_128[0], len);
  input_128 = (vector __int128_t){12345678};

  if (vec_s128_expected1[0] != vec_s128_result1[0])
    {
#ifdef DEBUG
       printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_s128_result1[0] = %lld %llu; ",
	      len, vec_s128_result1[0] >> 64,
	      vec_s128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
       printf("vec_s128_expected1[0] = %lld %llu\n",
	      vec_s128_expected1[0] >> 64,
	      vec_s128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
#else
       abort ();
#endif
    }

  len = 16;
  vec_u128_expected1 = (vector __uint128_t){12800001};
  input_u128 = (vector __uint128_t){12800001};
  vec_u128_result1 = vec_xl_len (&input_u128[0], len);
  input_u128 = (vector __uint128_t){98765432};

  if (vec_u128_expected1[0] != vec_u128_result1[0])
#ifdef DEBUG
    {
       printf("Error: pre/post initialzation vec_xl_len(), len = %d, vec_u128_result1[0] = %lld; ",
	      len, vec_u128_result1[0] >> 64,
	      vec_u128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
       printf("vec_u128_expected1[0] = %lld\n",
	      vec_u128_expected1[0] >> 64,
	      vec_u128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
    }
#else
    abort ();
#endif

    /* Tests to ensure the vec_xl_len_r() is properly optimized with a pre
       and post data initialization is done.  */

  len = 16;
  vec_uc_expected1 = (vector unsigned char){ 16,15, 14, 13, 12, 11, 10, 9,
					     8, 7, 6, 5, 4, 3, 2, 1 };
  input_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8, 9,
				   10, 11, 12, 13, 14, 15, 16 };
  vec_uc_result1 = vec_xl_len_r (&input_uc[0], len);
  input_uc = (vector unsigned char){ 10, 11, 12, 13, 14, 15, 16,
				     17, 18, 19, 20, 21, 22, 23, 24 };

  for (i = 0; i < 16; i++)
    {
      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: pre/post initialzation vec_xl_len_r(), len = %d, vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       len, i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif
    }

}



























