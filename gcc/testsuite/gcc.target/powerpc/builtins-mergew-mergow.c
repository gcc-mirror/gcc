/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h> // vector
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main() {
  vector signed int vec_si_arg1, vec_si_arg2;
  vector signed int vec_si_expected, vec_si_result;
  vector unsigned int vec_ui_arg1, vec_ui_arg2;
  vector unsigned int vec_ui_expected, vec_ui_result;
  vector signed long long vec_sll_arg1, vec_sll_arg2;
  vector signed long long vec_sll_expected, vec_sll_result;
  vector unsigned long long vec_ull_arg1, vec_ull_arg2;
  vector unsigned long long vec_ull_expected, vec_ull_result;
  vector bool long long vec_bll_arg1, vec_bll_arg2;
  vector bool long long vec_bll_expected, vec_bll_result;
  vector float vec_f_arg1, vec_f_arg2;
  vector float vec_f_expected, vec_f_result;
  vector double vec_d_arg1, vec_d_arg2;
  vector double vec_d_expected, vec_d_result;
  int i;
  unsigned long long int value;

  /* Tests for vec_mergeo, Merges the odd-numbered halves of two vectors.  */
  vec_si_arg1 = (vector int){-100, -101, 102, 103};
  vec_si_arg2 = (vector int){200, 201, 202, 203};

  vec_si_result = vec_mergeo (vec_si_arg1, vec_si_arg2);

  vec_si_expected = (vector int){-101, 201, 103, 203};

  for (i = 0; i < 4; i++)
    if (vec_si_result[i] != vec_si_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_si_result[%d] = %d, vec_si_expected[%d] = %d\n",
	     i, vec_si_result[i], i, vec_si_expected[i]);
#else
      abort();
#endif

  vec_ui_arg1 = (vector unsigned int){100, 101, 102, 103};
  vec_ui_arg2 = (vector unsigned int){200, 201, 202, 203};

  vec_ui_result = vec_mergeo (vec_ui_arg1, vec_ui_arg2);

  vec_ui_expected = (vector unsigned int){101, 201, 103, 203};

  for (i = 0; i < 4; i++)
    if (vec_ui_result[i] != vec_ui_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_ui_result[%d] = %d, vec_ui_expected[%d] = %d\n",
	     i, vec_ui_result[i], i, vec_ui_expected[i]);
#else
      abort();
#endif

  vec_sll_arg1 = (vector long long int){-300, -301};
  vec_sll_arg2 = (vector long long int){400, 401};

  vec_sll_result = vec_mergeo (vec_sll_arg1, vec_sll_arg2);

  vec_sll_expected = (vector long long int){-301, 401};

  for (i = 0; i < 2; i++)
    if (vec_sll_result[i] != vec_sll_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_sll_result[%d] = %lld, vec_sll_expected[%d] = %lld\n",
	     i, (long long int)vec_sll_result[i],
	     i, (long long int)vec_sll_expected[i]);
#else
      abort();
#endif

  vec_ull_arg1 = (vector unsigned long long int){500, 501};
  vec_ull_arg2 = (vector unsigned long long int){600, 601};

  vec_ull_result = vec_mergeo (vec_ull_arg1, vec_ull_arg2);

  vec_ull_expected = (vector unsigned long long int){501, 601};

  for (i = 0; i < 2; i++)
    if (vec_ull_result[i] != vec_ull_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_ull_result[%d] = %lld, vec_ull_expected[%d] = %lld\n",
	     i, (unsigned long long int)vec_ull_result[i],
	     i, (unsigned long long int)vec_ull_expected[i]);
#else
      abort();
#endif

  vec_bll_arg1 = (vector bool long long){0, 0};
  vec_bll_arg2 = (vector bool long long){1, 1};

  vec_bll_result = vec_mergeo (vec_bll_arg1, vec_bll_arg2);

  vec_bll_expected = (vector bool long long){0, 1};

  for (i = 0; i < 2; i++)
    if (vec_bll_result[i] != vec_bll_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_bll_result[%d] = %lld, vec_bll_expected[%d] = %lld\n",
	     i, vec_ull_result[i],
	     i, vec_ull_expected[i]);
#else
      abort();
#endif

  vec_f_arg1 = (vector float){100.0, 101.1, 102.2, 103.3};
  vec_f_arg2 = (vector float){200.0, 201.1, 202.2, 203.3};

  vec_f_result = vec_mergeo (vec_f_arg1, vec_f_arg2);

  vec_f_expected = (vector float){101.1, 201.1, 103.3, 203.3};

  for (i = 0; i < 4; i++)
    if (vec_f_result[i] != vec_f_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_f_result[%d] = %f, vec_f_expected[%d] = %f\n",
	     i, vec_f_result[i], i, vec_f_expected[i]);
#else
      abort();
#endif

  vec_d_arg1 = (vector double){300.0, 301.1};
  vec_d_arg2 = (vector double){400.0, 401.1};

  vec_d_result = vec_mergeo (vec_d_arg1, vec_d_arg2);

  vec_d_expected = (vector double){301.1, 401.1};

  for (i = 0; i < 2; i++)
    if (vec_d_result[i] != vec_d_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergeo(): vec_d_result[%d] = %f, vec_d_expected[%d] = %f\n",
	     i, vec_d_result[i], i, vec_d_expected[i]);
#else
      abort();
#endif

  /* Tests for vec_mergee, Merges the even-numbered halves of two vectors.  */
  vec_si_arg1 = (vector int){-100, -101, 102, 103};
  vec_si_arg2 = (vector int){200, 201, 202, 203};

  vec_si_result = vec_mergee (vec_si_arg1, vec_si_arg2);

  vec_si_expected = (vector int){-100, 200, 102, 202};

  for (i = 0; i < 4; i++)
    if (vec_si_result[i] != vec_si_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergee(): vec_si_result[%d] = %d, vec_si_expected[%d] = %d\n",
	     i, vec_si_result[i], i, vec_si_expected[i]);
#else
      abort();
#endif

  vec_ui_arg1 = (vector unsigned int){100, 101, 102, 103};
  vec_ui_arg2 = (vector unsigned int){200, 201, 202, 203};

  vec_ui_result = vec_mergee (vec_ui_arg1, vec_ui_arg2);

  vec_ui_expected = (vector unsigned int){100, 200, 102, 202};

  for (i = 0; i < 4; i++)
    if (vec_ui_result[i] != vec_ui_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergee(): vec_ui_result[%d] = %d, vec_ui_expected[%d] = %d\n",
	     i, vec_ui_result[i], i, vec_ui_expected[i]);
#else
      abort();
#endif

  vec_sll_arg1 = (vector signed long long int){-300, -301};
  vec_sll_arg2 = (vector signed long long int){400, 401};

  vec_sll_result = vec_mergee (vec_sll_arg1, vec_sll_arg2);

  vec_sll_expected = (vector signed long long int){-300, 400};

  for (i = 0; i < 2; i++)
    if (vec_sll_result[i] != vec_sll_expected[i])
#ifdef DEBUG
	printf("ERROR vec_mergee(): vec_sll_result[%d] = %lld, vec_sll_expected[%d] = %lld\n",
		i, (signed long long int)vec_sll_result[i],
		i, (signed long long int)vec_sll_expected[i]);
#else
      abort();
#endif

  vec_ull_arg1 = (vector unsigned long long int){500, 501};
  vec_ull_arg2 = (vector unsigned long long int){600, 601};

  vec_ull_result = vec_mergee (vec_ull_arg1, vec_ull_arg2);

  vec_ull_expected = (vector unsigned long long int){500, 600};

  for (i = 0; i < 2; i++)
    if (vec_ull_result[i] != vec_ull_expected[i])
#ifdef DEBUG
	printf("ERROR vec_mergee(): vec_ull_result[%d] = %lld, vec_ull_expected[%d] = %lld\n",
	       i, (unsigned long long int)vec_ull_result[i],
	       i, (unsigned long long int)vec_ull_expected[i]);
#else
      abort();
#endif

  vec_bll_arg1 = (vector bool long long){0, 0};
  vec_bll_arg2 = (vector bool long long){1, 1};

  vec_bll_result = vec_mergee (vec_bll_arg1, vec_bll_arg2);

  vec_bll_expected = (vector bool long long){0, 1};

  for (i = 0; i < 2; i++)
    if (vec_bll_result[i] != vec_bll_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergee(): vec_bll_result[%d] = %lld, vec_bll_expected[%d] = %lld\n",
	     i, vec_ull_result[i],
	     i, vec_ull_expected[i]);
#else
      abort();
#endif

  vec_f_arg1 = (vector float){100.0, 101.1, 102.2, 103.3};
  vec_f_arg2 = (vector float){200.0, 201.1, 202.2, 203.3};

  vec_f_result = vec_mergee (vec_f_arg1, vec_f_arg2);

  vec_f_expected = (vector float){100.0, 200.0, 102.2, 202.2};

  for (i = 0; i < 4; i++)
    if (vec_f_result[i] != vec_f_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergee(): vec_f_result[%d] = %f, vec_f_expected[%d] = %f\n",
	     i, vec_f_result[i], i, vec_f_expected[i]);
#else
      abort();
#endif

  vec_d_arg1 = (vector double){300.0, 301.1};
  vec_d_arg2 = (vector double){400.0, 401.1};

  vec_d_result = vec_mergee (vec_d_arg1, vec_d_arg2);

  vec_d_expected = (vector double){300.0, 400.0};

  for (i = 0; i < 2; i++)
    if (vec_d_result[i] != vec_d_expected[i])
#ifdef DEBUG
      printf("ERROR vec_mergee(): vec_d_result[%d] = %f, vec_d_expected[%d] = %f\n",
	     i, vec_d_result[i], i, vec_d_expected[i]);
#else
      abort();
#endif
}
