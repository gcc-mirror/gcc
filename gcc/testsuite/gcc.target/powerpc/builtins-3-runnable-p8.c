/* { dg-do run { target { powerpc*-*-linux* && { p8vector_hw } } } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8" } */

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
			printf("vec_result[%d] (%f) != vec_expected[%d] (%f)\n",
			       i, vec_result[i], i, vec_expected[i]);
#else
			abort();
#endif
		}
	}
}

int main()
{
	int i;
	vector unsigned int vec_unint, vec_uns_int_expected, vec_uns_int_result;
	vector signed int vec_int, vec_int_expected, vec_int_result;
	vector float  vec_flt, vec_flt_result, vec_flt_expected;
	vector long long int vec_ll_int0, vec_ll_int1;
	vector long long unsigned int vec_ll_uns_int0, vec_ll_uns_int1;
	vector double vec_dble0, vec_dble1, vec_dble_result, vec_dble_expected;

	vec_ll_int0 = (vector long long int){ -12, -12345678901234 };
	vec_ll_int1 = (vector long long int){ 12, 9876543210 };
	vec_ll_uns_int0 = (vector unsigned long long int){ 102, 9753108642 };
	vec_ll_uns_int1 = (vector unsigned long long int){ 23, 29 };

	/* Convert two double precision vector float to vector int */
	vec_dble0 = (vector double){-124.930, 81234.49};
	vec_dble1 = (vector double){-24.370, 8354.99};
	vec_int_expected = (vector signed int){-124, 81234, -24, 8354};
	vec_int_result = vec_signed2 (vec_dble0, vec_dble1);
	test_int_result (ALL, vec_int_result, vec_int_expected);

	/* Convert two double precision vector float to vector unsigned int */
	vec_dble0 = (vector double){124.930, 8134.49};
	vec_dble1 = (vector double){24.370, 834.99};
	vec_uns_int_expected = (vector unsigned int){124, 8134, 24, 834};
	vec_uns_int_result = vec_unsigned2 (vec_dble0, vec_dble1);
	test_unsigned_int_result (ALL, vec_uns_int_result,
				  vec_uns_int_expected);

	/* conversion of two double precision vectors to single precision vector */
	vec_flt_expected = (vector float){-12.00, -12345678901234.00, 12.00, 9876543210.00};
	vec_flt_result = vec_float2 (vec_ll_int0, vec_ll_int1);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	vec_flt_expected = (vector float){102.00, 9753108642.00, 23.00, 29.00};
	vec_flt_result = vec_float2 (vec_ll_uns_int0, vec_ll_uns_int1);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);

	vec_dble0 = (vector double){ 34.0, 97.0 };
	vec_dble1 = (vector double){ 214.0, -5.5 };
	vec_flt_expected = (vector float){34.0, 97.0, 214.0, -5.5};
	vec_flt_result = vec_float2 (vec_dble0, vec_dble1);
	test_result_sp(ALL, vec_flt_result, vec_flt_expected);
}

