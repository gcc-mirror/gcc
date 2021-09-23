/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mxscvsqqp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxscvuqqp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxscvqpsqz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxscvqpuqz\M} 1 } } */

#include <stdio.h>
#include <math.h>
#include <fenv.h>
#include <stdlib.h>
#include <wchar.h>

#define DEBUG 0

void
abort (void);

float
conv_i_2_fp( long long int a)
{
  return (float) a;
}

double
conv_i_2_fpd( long long int a)
{
  return (double) a;
}

double
conv_ui_2_fpd( unsigned long long int a)
{
  return (double) a;
}

__float128
conv_i128_2_fp128 (__int128_t a)
{
  // default, gen inst KF mode
  // -mabi=ibmlongdouble, gen inst floattiieee KF mode
  // -mabi=ieeelongdouble gen inst floattiieee TF mode
  return (__float128) a;
}

__float128
conv_ui128_2_fp128 (__uint128_t a)
{
  // default, gen inst KF mode
  // -mabi=ibmlongdouble, gen inst floattiieee KF mode
  // -mabi=ieeelongdouble gen inst floattiieee TF mode
  return (__float128) a;
}

__int128_t
conv_fp128_2_i128 (__float128 a)
{
  // default, gen inst KF mode
  // -mabi=ibmlongdouble, gen inst floattiieee KF mode
  // -mabi=ieeelongdouble gen inst floattiieee TF mode
  return (__int128_t) a;
}

__uint128_t
conv_fp128_2_ui128 (__float128 a)
{
  // default, gen inst KF mode
  // -mabi=ibmlongdouble, gen inst floattiieee KF mode
  // -mabi=ieeelongdouble gen inst floattiieee TF mode
  return (__uint128_t) a;
}

long double
conv_i128_2_ld (__int128_t a)
{
  // default, gen call __floattitf
  // -mabi=ibmlongdouble, gen call __floattitf
  // -mabi=ieeelongdouble gen inst floattiieee TF mode
  return (long double) a;
}

__ibm128
conv_i128_2_ibm128 (__int128_t a)
{
  // default, gen call __floattitf
  // -mabi=ibmlongdouble, gen call __floattitf
  // -mabi=ieeelongdouble, message uses IBM long double, no binary output
  return (__ibm128) a;
}

int
main()
{
	float a, expected_result_float;
	double b, expected_result_double;
	long long int c, expected_result_llint;
	unsigned long long int u;
	__int128_t d;
	__uint128_t u128;
	unsigned long long expected_result_uint128[2] ;
	__float128 e;
	long double ld;     // another 128-bit float version

	union conv_t {
		float a;
		double b;
		long long int c;
		long long int128[2] ;
		unsigned long long uint128[2] ;
		unsigned long long int u;
		__int128_t d;
		__uint128_t u128;
		__float128 e;
		long double ld;     // another 128-bit float version
	} conv, conv_result;

	c = 20;
	expected_result_llint = 20.00000;
	a = conv_i_2_fp (c);

	if (a != expected_result_llint) {
#if DEBUG
		printf("ERROR: conv_i_2_fp(%lld) = %10.5f\n", c, a);
		printf("\n does not match expected_result = %10.5f\n\n",
				 expected_result_llint);
#else
		abort();
#endif
	}

	c = 20;
	expected_result_double = 20.00000;
	b = conv_i_2_fpd (c);

	if (b != expected_result_double) {
#if DEBUG
		printf("ERROR: conv_i_2_fpd(%lld) = %10.5f\n", d, b);
		printf("\n does not match expected_result = %10.5f\n\n",
				 expected_result_double);
 #else
		abort();
#endif
	}

	u = 20;
	expected_result_double = 20.00000;
	b = conv_ui_2_fpd (u);

	if (b != expected_result_double) {
#if DEBUG
		printf("ERROR: conv_ui_2_fpd(%llu) = %10.5f\n", u, b);
		printf("\n does not match expected_result = %10.5f\n\n",
				 expected_result_double);
 #else
		abort();
#endif
	}

  d = -3210;
  d = (d * 10000000000) + 9876543210;
  conv_result.e = conv_i128_2_fp128 (d);
  expected_result_uint128[1] = 0xc02bd2f9068d1160;
  expected_result_uint128[0] = 0x0;
  
  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_i128_2_fp128(-32109876543210) = (result in hex) 0x%llx %llx\n",
				conv.uint128[1], conv.uint128[0]);
	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  d = 123;
  d = (d * 10000000000) + 1234567890;
  conv_result.ld = conv_i128_2_fp128 (d);
  expected_result_uint128[1] = 0x0;
  expected_result_uint128[0] = 0x4271eab4c8ed2000;

  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_i128_2_fp128(1231234567890) = (result in hex) 0x%llx %llx\n",
				conv.uint128[1], conv.uint128[0]);
	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  u128 = 8760;
  u128 = (u128 * 10000000000) + 1234567890;
  conv_result.e = conv_ui128_2_fp128 (u128);
  expected_result_uint128[1] = 0x402d3eb101df8b48;
  expected_result_uint128[0] = 0x0;

  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_ui128_2_fp128(87601234567890) = (result in hex) 0x%llx %llx\n",
				conv.uint128[1], conv.uint128[0]);
	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  u128 = 3210;
  u128 = (u128 * 10000000000) + 9876543210;
  expected_result_uint128[1] = 0x402bd3429c8feea0;
  expected_result_uint128[0] = 0x0;
  conv_result.e = conv_ui128_2_fp128 (u128);

  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_ui128_2_fp128(32109876543210) = (result in hex) 0x%llx %llx\n",
				conv.uint128[1], conv.uint128[0]);
	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  conv.e = 12345.6789;
  expected_result_uint128[1] = 0x1407374883526960;
  expected_result_uint128[0] = 0x3039;

  conv_result.d = conv_fp128_2_i128 (conv.e);

  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_fp128_2_i128(0x%llx %llx) =  ",
				conv.uint128[1], conv.uint128[0]);
	  printf("0x%llx %llx\n", conv_result.uint128[1], conv_result.uint128[0]);

	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  conv.e = -6789.12345;
  expected_result_uint128[1] = 0x0;
  expected_result_uint128[0] = 0xffffffffffffe57b;
  conv_result.d = conv_fp128_2_i128 (conv.e);
 
  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_fp128_2_i128(0x%llx %llx) = ",
				conv.uint128[1], conv.uint128[0]);
	  printf("0x%llx %llx\n", conv_result.uint128[1], conv_result.uint128[0]);

	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  conv.e = 6789.12345;
  expected_result_uint128[1] = 0x0;
  expected_result_uint128[0] = 0x1a85;
  conv_result.d = conv_fp128_2_ui128 (conv.e);
 
  if ((conv_result.uint128[1] != expected_result_uint128[1])
		&& (conv_result.uint128[0] != expected_result_uint128[0])) {
#if DEBUG
	  printf("ERROR: conv_fp128_2_ui128(0x%llx %llx) = ",
				conv.uint128[1], conv.uint128[0]);
	  printf("0x%llx %llx\n", conv_result.uint128[1], conv_result.uint128[0]);
	  
	  printf("\n does not match expected_result = (result in hex) 0x%llx %llx\n\n",
				expected_result_uint128[1], expected_result_uint128[0]);
 #else
	  abort();
#endif
	}

  return 0;
}
