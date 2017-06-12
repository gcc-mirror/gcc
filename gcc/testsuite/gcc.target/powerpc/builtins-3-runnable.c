/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx -mcpu=power8" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */

#include <altivec.h> // vector

void abort (void);

void test_result_dp(vector double vec_result, vector double vec_expected)
{
	if (vec_result[0] != vec_expected[0])
		abort();

	if (vec_result[1] != vec_expected[1])
		abort();
}

int main()
{
	int i;
	vector unsigned int vec_unint;
	vector signed int vec_int;
	vector float  vec_flt, vec_flt_result, vec_flt_expected;
	vector double vec_dble0, vec_dble1, vec_dble_result, vec_dble_expected;

	vec_int = (vector signed int){ -1, 3, -5, 1234567 };
	vec_unint = (vector unsigned int){ 9, 11, 15, 2468013579 };
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

}
