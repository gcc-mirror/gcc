/* { dg-do run { target { powerpc*-*-* && p8vector_hw } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 " } */

/* Make sure the test case compiled with -O2 generates the same expected
   results.  The expected results were generated with -O0.  */

#include <altivec.h>
#define TRUE 1
#define FALSE 0

#define DEBUG 1

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

typedef vector unsigned long long	crypto_t;
typedef vector unsigned long long	v2di_t;
typedef vector unsigned int		v4si_t;
typedef vector unsigned short		v8hi_t;
typedef vector unsigned char		v16qi_t;

v16qi_t crypto6a (v16qi_t a, v16qi_t b, v16qi_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v8hi_t crypto6b (v8hi_t a, v8hi_t b, v8hi_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v4si_t crypto6c (v4si_t a, v4si_t b, v4si_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v2di_t crypto6d (v2di_t a, v2di_t b, v2di_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

int main()
{
  int i;
  v16qi_t expected_v16qi, result_v16qi;
  v8hi_t expected_v8hi, result_v8hi;
  v4si_t expected_v4si, result_v4si;
  v2di_t expected_v2di, result_v2di;
  v16qi_t v16qi_arg_a, v16qi_arg_b, v16qi_arg_c;
  v8hi_t v8hi_arg_a, v8hi_arg_b, v8hi_arg_c;
  v4si_t v4si_arg_a, v4si_arg_b, v4si_arg_c;
  v2di_t v2di_arg_a, v2di_arg_b, v2di_arg_c;

  v16qi_arg_a = (vector unsigned char){ 7, 6, 5, 4, 3, 2, 1, 0,
					1, 2, 3, 4, 5, 6, 7, 8 };
  v16qi_arg_b = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					7, 6, 5, 4, 3, 2, 1, 0 };
  v16qi_arg_c = (vector unsigned char){ 7, 2, 5, 4, 3, 6, 1, 8,
					1, 6, 3, 4, 5, 2, 7, 0 };
  expected_v16qi = (vector unsigned char){ 15, 10, 13, 12, 11, 14, 9, 0,
					   9, 14, 11, 12, 13, 10, 15, 8 };
		
  result_v16qi = crypto6a (v16qi_arg_a, v16qi_arg_b, v16qi_arg_c);
				      
  for (i = 0; i < 16; i++)
    if (expected_v16qi[i] != result_v16qi[i])
      printf("crypto6a: result_v16qi[%d] =  %d, expected = %d\n",
	     i, result_v16qi[i], expected_v16qi[i]);

  v8hi_arg_a = (vector unsigned short int){ 7, 6, 5, 4, 3, 2, 1, 0};
  v8hi_arg_b = (vector unsigned short int){ 1, 2, 3, 4, 5, 6, 7, 8};
  v8hi_arg_c = (vector unsigned short int){ 7, 2, 5, 4, 3, 6, 1, 8};
  expected_v8hi = (vector unsigned short int){ 5, 0, 6, 0, 7, 0, 8};
		
  result_v8hi = crypto6b (v8hi_arg_a, v8hi_arg_b, v8hi_arg_c);
				      
  for (i = 0; i < 8; i++)
    if (expected_v8hi[i] != result_v8hi[i])
      printf("crypto6a: result_v8hi[%d] =  %d, expected = %d\n",
	     i, result_v8hi[i], expected_v8hi[i]);

  v4si_arg_a = (vector unsigned int){ 7, 6, 5, 4};
  v4si_arg_b = (vector unsigned int){ 15, 6, 7, 8};
  v4si_arg_c = (vector unsigned int){ 7, 14, 3, 6};
  expected_v4si = (vector unsigned int){ 7, 0, 8, 0};
		
  result_v4si = crypto6c (v4si_arg_a, v4si_arg_b, v4si_arg_c);
				      
  for (i = 0; i < 4; i++)
    if (expected_v4si[i] != result_v4si[i])
      printf("crypto6a: result_v4si[%d] =  %d, expected = %d\n",
	     i, result_v4si[i], expected_v4si[i]);

  v2di_arg_a = (vector unsigned long long int){ 7, 6, };
  v2di_arg_b = (vector unsigned long long int){ 15, 6, };
  v2di_arg_c = (vector unsigned long long int){ 7, 14};
  expected_v2di = (vector unsigned long long int){ 6, 0};
		
  result_v2di = crypto6d (v2di_arg_a, v2di_arg_b, v2di_arg_c);
				      
  for (i = 0; i < 2; i++)
    if (expected_v2di[i] != result_v2di[i])
      printf("crypto6a: result_v2di[%d] =  %d, expected = %d\n",
	     i, result_v2di[i], expected_v2di[i]);
}
