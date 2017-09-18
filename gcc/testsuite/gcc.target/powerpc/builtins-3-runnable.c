/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx -mcpu=power8" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */

#include <altivec.h> // vector

#ifdef DEBUG
#include <stdio.h>
#endif

#define ALL  1
#define EVEN 2
#define ODD  3

void abort (void);

void test_int_result(int check, vector int vec_result, vector int vec_expected)
{
	int i;

	for (i = 0; i < 4; i++) {
		switch (check) {
		case ALL:
			break;
		case EVEN:
			if (i%2 == 0)
				break;
			else
				continue;
		case ODD:
			if (i%2 != 0)
				break;
			else
				continue;
		}

		if (vec_result[i] != vec_expected[i]) {
#ifdef DEBUG
			printf("Test_int_result: ");
			printf("vec_result[%d] (%d) != vec_expected[%d] (%d)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}
	}
}

void test_unsigned_int_result(int check, vector unsigned int vec_result,
			      vector unsigned int vec_expected)
{
	int i;

	for (i = 0; i < 4; i++) {
		switch (check) {
		case ALL:
			break;
		case EVEN:
			if (i%2 == 0)
				break;
			else
				continue;
		case ODD:
			if (i%2 != 0)
				break;
			else
				continue;
		}

		if (vec_result[i] != vec_expected[i]) {
#ifdef DEBUG
			printf("Test_unsigned int_result: ");
			printf("vec_result[%d] (%d) != vec_expected[%d] (%d)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}

	}
}

void test_ll_int_result(vector long long int vec_result,
			vector long long int vec_expected)
{
	int i;

	for (i = 0; i < 2; i++)
		if (vec_result[i] != vec_expected[i]) {
#ifdef DEBUG
			printf("Test_ll_int_result: ");
			printf("vec_result[%d] (%lld) != vec_expected[%d] (%lld)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}
}

void test_ll_unsigned_int_result(vector long long unsigned int vec_result,
				 vector long long unsigned int vec_expected)
{
	int i;

	for (i = 0; i < 2; i++)
		if (vec_result[i] != vec_expected[i]) {
#ifdef DEBUG
			printf("Test_ll_unsigned_int_result: ");
			printf("vec_result[%d] (%lld) != vec_expected[%d] (%lld)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}
}

void test_result_sp(int check, vector float vec_result,
		    vector float vec_expected)
{
	int i;
	for(i = 0; i<4; i++) {

		switch (check) {
		case ALL:
			break;
		case EVEN:
			if (i%2 == 0)
				break;
			else
				continue;
		case ODD:
			if (i%2 != 0)
				break;
			else
				continue;
		}

		if (vec_result[i] != vec_expected[i]) {
#ifdef DEBUG
			printf("Test_result_sp: ");
			printf("vec_result[%d] (%lld) != vec_expected[%d] (%lld)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}
	}
}

void test_result_dp(vector double vec_result, vector double vec_expected)
{
	if (vec_result[0] != vec_expected[0]) {
#ifdef DEBUG
		printf("Test_result_dp: ");
		printf("vec_result[0] (%lld) != vec_expected[0] (%lld)\n",
		       vec_result[0], vec_expected[0]);
#else
		abort();
#endif
	}

	if (vec_result[1] != vec_expected[1]) {
#ifdef DEBUG
		printf("Test_result_dp: ");
		printf("vec_result[1] (%lld) != vec_expected[1] (%lld)\n",
		       vec_result[1], vec_expected[1]);
#else
		abort();
#endif
	}
}

int main()
{
	int i;
	vector unsigned int vec_unint, vec_uns_int_expected, vec_uns_int_result;
	vector signed int vec_int, vec_int_expected, vec_int_result;
	vector long long int vec_ll_int0, vec_ll_int1;
	vector long long int vec_ll_int_expected, vec_ll_int_result;
	vector long long unsigned int vec_ll_uns_int0, vec_ll_uns_int1;
	vector long long unsigned int vec_ll_uns_int_expected, vec_ll_uns_int_result;
	vector float  vec_flt, vec_flt_result, vec_flt_expected;
	vector double vec_dble0, vec_dble1, vec_dble_result, vec_dble_expected;

	vec_int = (vector signed int){ -1, 3, -5, 1234567 };
	vec_ll_int0 = (vector long long int){ -12, -12345678901234 };
	vec_ll_int1 = (vector long long int){ 12, 9876543210 };
	vec_unint = (vector unsigned int){ 9, 11, 15, 2468013579 };
	vec_ll_uns_int0 = (vector unsigned long long int){ 102, 9753108642 };
	vec_ll_uns_int1 = (vector unsigned long long int){ 23, 29 };
	vec_flt = (vector float){ -21., 3.5, -53., 78. };
	vec_dble0 = (vector double){ 34.0, 97.0 };
	vec_dble1 = (vector double){ 214.0, -5.5 };

	/* conversion of words 0 and 2 */
	vec_dble_expected = (vector double){-1.000000, -5.000000};
	vec_dble_result = vec_doublee (vec_int);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){9.000000, 15.000000};
	vec_dble_result = vec_doublee (vec_unint);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){-21.000000, -53.000000};
	vec_dble_result = vec_doublee (vec_flt);
	test_result_dp(vec_dble_result, vec_dble_expected);


	/* conversion of words 1 and 3 */
	vec_dble_expected = (vector double){3.000000, 1234567.000000};
	vec_dble_result = vec_doubleo (vec_int);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){11.000000, 2468013579.000000};
	vec_dble_result = vec_doubleo (vec_unint);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){3.500000, 78.000000};
	vec_dble_result = vec_doubleo (vec_flt);
	test_result_dp(vec_dble_result, vec_dble_expected);


	/* conversion of words 0 and 1 */
	vec_dble_expected = (vector double){-5.000000, 1234567.000000};
	vec_dble_result = vec_doublel (vec_int);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){15.000000, 2468013579.000000};
	vec_dble_result = vec_doublel (vec_unint);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){-53.000000, 78.000000};
	vec_dble_result = vec_doublel (vec_flt);
	test_result_dp(vec_dble_result, vec_dble_expected);


	/* conversion of words 2 and 3 */
	vec_dble_expected = (vector double){-1.000000, 3.000000};
	vec_dble_result = vec_doubleh (vec_int);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){9.000000, 11.000000};
	vec_dble_result = vec_doubleh (vec_unint);
	test_result_dp(vec_dble_result, vec_dble_expected);

	vec_dble_expected = (vector double){-21.000000, 3.500000};
	vec_dble_result = vec_doubleh (vec_flt);
	test_result_dp(vec_dble_result, vec_dble_expected);

	/* conversion of integer vector to single precision float vector */
	vec_flt_expected = (vector float){-1.00, 3.00, -5.00, 1234567.00};
	vec_flt_result = vec_float (vec_int);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){9.00, 11.00, 15.00, 2468013579.0};
	vec_flt_result = vec_float (vec_unint);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	/* conversion of two double precision vectors to single precision vector */
	vec_flt_expected = (vector float){-12.00, -12345678901234.00, 12.00, 9876543210.00};
	vec_flt_result = vec_float2 (vec_ll_int0, vec_ll_int1);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){102.00, 9753108642.00, 23.00, 29.00};
	vec_flt_result = vec_float2 (vec_ll_uns_int0, vec_ll_uns_int1);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	/* conversion of even words in double precision vector to single precision vector */
	vec_flt_expected = (vector float){-12.00, 00.00, -12345678901234.00, 0.00};
	vec_flt_result = vec_floate (vec_ll_int0);
	test_result_sp(EVEN, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){102.00, 0.00, 9753108642.00, 0.00};
	vec_flt_result = vec_floate (vec_ll_uns_int0);
	test_result_sp(EVEN, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){34.00, 0.00, 97.00, 0.00};
	vec_flt_result = vec_floate (vec_dble0);
	test_result_sp(EVEN, vec_flt_result, vec_flt_expected);

	/* conversion of odd words in double precision vector to single precision vector */
	vec_flt_expected = (vector float){0.00, -12.00, 00.00, -12345678901234.00};
	vec_flt_result = vec_floato (vec_ll_int0);
	test_result_sp(ODD, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){0.00, 102.00, 0.00, 9753108642.00};
	vec_flt_result = vec_floato (vec_ll_uns_int0);
	test_result_sp(ODD, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){0.00, 34.00, 0.00, 97.00};
	vec_flt_result = vec_floato (vec_dble0);
	test_result_sp(ODD, vec_flt_result, vec_flt_expected);

	/* Convert single precision float to int */
	vec_flt = (vector float){-14.30, 34.00, 22.00, 97.00};
	vec_int_expected = (vector signed int){-14, 34, 22, 97};
	vec_int_result = vec_signed (vec_flt);
	test_int_result (ALL, vec_int_result, vec_int_expected);

	/* Convert double precision float to long long int */
	vec_dble0 = (vector double){-124.930, 81234.49};
	vec_ll_int_expected = (vector long long signed int){-124, 81234};
	vec_ll_int_result = vec_signed (vec_dble0);
	test_ll_int_result (vec_ll_int_result, vec_ll_int_expected);

	/* Convert two double precision vector float to vector int */
	vec_dble0 = (vector double){-124.930, 81234.49};
	vec_dble1 = (vector double){-24.370, 8354.99};
	vec_int_expected = (vector signed int){-124, 81234, -24, 8354};
	vec_int_result = vec_signed2 (vec_dble0, vec_dble1);
	test_int_result (ALL, vec_int_result, vec_int_expected);

	/* Convert double precision vector float to vector int, even words */
	vec_dble0 = (vector double){-124.930, 81234.49};
	vec_int_expected = (vector signed int){-124, 0, 81234, 0};
	vec_int_result = vec_signede (vec_dble0);
	test_int_result (EVEN, vec_int_result, vec_int_expected);

	/* Convert double precision vector float to vector int, odd words */
	vec_dble0 = (vector double){-124.930, 81234.49};
	vec_int_expected = (vector signed int){0, -124, 0, 81234};
	vec_int_result = vec_signedo (vec_dble0);
	test_int_result (ODD, vec_int_result, vec_int_expected);

	/* Convert double precision float to long long unsigned int */
	vec_dble0 = (vector double){124.930, 8134.49};
	vec_ll_uns_int_expected = (vector long long unsigned int){124, 8134};
	vec_ll_uns_int_result = vec_unsigned (vec_dble0);
	test_ll_unsigned_int_result (vec_ll_uns_int_result,
				     vec_ll_uns_int_expected);

	/* Convert two double precision vector float to vector unsigned int */
	vec_dble0 = (vector double){124.930, 8134.49};
	vec_dble1 = (vector double){24.370, 834.99};
	vec_uns_int_expected = (vector unsigned int){124, 8134, 24, 834};
	vec_uns_int_result = vec_unsigned2 (vec_dble0, vec_dble1);
	test_unsigned_int_result (ALL, vec_uns_int_result,
				  vec_uns_int_expected);

	/* Convert double precision vector float to vector unsigned int,
	   even words */
	vec_dble0 = (vector double){3124.930, 8234.49};
	vec_uns_int_expected = (vector unsigned int){3124, 0, 8234, 0};
	vec_uns_int_result = vec_unsignede (vec_dble0);
	test_unsigned_int_result (EVEN, vec_uns_int_result,
				  vec_uns_int_expected);

	/* Convert double precision vector float to vector unsigned int,
	   odd words */
	vec_dble0 = (vector double){1924.930, 81234.49};
	vec_uns_int_expected = (vector unsigned int){0, 1924, 0, 81234};
	vec_uns_int_result = vec_unsignedo (vec_dble0);
	test_unsigned_int_result (ODD, vec_uns_int_result,
				  vec_uns_int_expected);
}

