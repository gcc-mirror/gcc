/*
   Test of vec_xl_sext and vec_xl_zext (load into rightmost
   vector element and zero/sign extend). */

/* { dg-do run { target power10_hw } } */
/* { dg-do compile { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */

/* Deliberately set optization to zero for this test to confirm
   the lxvr*x instruction is generated. At higher optimization levels
   the instruction we are looking for is sometimes replaced by other
   load instructions.  */
/* { dg-options "-mdejagnu-cpu=power10 -O0 -save-temps" } */

/* { dg-final { scan-assembler-times {\mlxvrhx\M} 2 } } */

#define NUM_VEC_ELEMS 8
#define ITERS 16

/*
Codegen at time of writing uses lxvrhx for the zero
extension test and lhax,mtvsrdd,vextsd2q for the
sign extended test.

0000000010001810 <test_sign_extended_load>:
    10001810:	ae 1a 24 7d 	lhax    r9,r4,r3
    10001814:	67 4b 40 7c 	mtvsrdd vs34,0,r9
    10001818:	02 16 5b 10 	vextsd2q v2,v2
    1000181c:	20 00 80 4e 	blr

0000000010001830 <test_zero_extended_unsigned_load>:
    10001830:	5b 18 44 7c 	lxvrhx  vs34,r4,r3
    10001834:	20 00 80 4e 	blr
*/

#include <altivec.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

long long buffer[8];
unsigned long verbose=0;

unsigned short initbuffer[32] = {
	0x1112, 0x1314, 0x1516, 0x1718,
			0x898a, 0x8b8c, 0x8d8e, 0x8f80,
	0x2122, 0x2324, 0x2526, 0x2728,
			0x999a, 0x9b9c, 0x9d9e, 0x9f90,
	0x3132, 0x3334, 0x3536, 0x3738,
			0xa9aa, 0xabac, 0xadae, 0xafa0,
	0x4142, 0x4344, 0x4546, 0x4748,
			0xb9ba, 0xbbbc, 0xbdbe, 0xbfb0
};

vector signed __int128 signed_expected[16] = {
	{ (__int128) 0x0 << 64 | (__int128) 0x1112},
	{ (__int128) 0x0 << 64 | (__int128) 0x1314},
	{ (__int128) 0x0 << 64 | (__int128) 0x1516},
	{ (__int128) 0x0 << 64 | (__int128) 0x1718},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff898a},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8b8c},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8d8e},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8f80},
	{ (__int128) 0x0 << 64 | (__int128) 0x2122},
	{ (__int128) 0x0 << 64 | (__int128) 0x2324},
	{ (__int128) 0x0 << 64 | (__int128) 0x2526},
	{ (__int128) 0x0 << 64 | (__int128) 0x2728},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff999a},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff9b9c},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff9d9e},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff9f90}
};

vector unsigned __int128 unsigned_expected[16] = {
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x1112},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x1314},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x1516},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x1718},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x898a},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x8b8c},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x8d8e},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x8f80},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x2122},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x2324},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x2526},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x2728},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x999a},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x9b9c},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x9d9e},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x9f90}
};

__attribute__ ((noinline))
vector signed __int128 test_sign_extended_load(int RA, signed short * RB) {
	return vec_xl_sext (RA, RB);
}

__attribute__ ((noinline))
vector unsigned __int128 test_zero_extended_unsigned_load(int RA, unsigned short * RB) {
	return vec_xl_zext (RA, RB);
}

int main (int argc, char *argv [])
{
   int iteration=0;
   int mismatch=0;
   vector signed __int128 signed_result_v;
   vector unsigned __int128 unsigned_result_v;
#if VERBOSE
   verbose=1;
   printf("%s %s\n", __DATE__, __TIME__);
#endif

  memcpy(&buffer, &initbuffer, sizeof(buffer));

   if (verbose) {
	   printf("input buffer:\n");
	   for (int k=0;k<32;k++) {
		   printf("%4x ",(unsigned short)initbuffer[k]);
		   if (k && (k+1)%4==0) printf("\n");
	   }
	   printf("signed_expected:\n");
	   for (int k=0;k<ITERS;k++) {
		printf("%llx",(unsigned long long)(signed_expected[k][0] >> 64));
		printf(" %llx \n",(unsigned long long)(signed_expected[k][0]));
		   printf("\n");
	   }
	   printf("unsigned_expected:\n");
	   for (int k=0;k<ITERS;k++) {
		printf("%llx ",(unsigned long long)(unsigned_expected[k][0]>>64));
		printf(" %llx \n",(unsigned long long)(unsigned_expected[k][0]));
		   printf("\n");
	   }
   }

   for (iteration = 0; iteration < ITERS ; iteration++ ) {
      signed_result_v = test_sign_extended_load (iteration*2, (signed short*)buffer);
      if (signed_result_v[0] != signed_expected[iteration][0] ) {
		mismatch++;
		printf("Unexpected results from signed load. i=%d \n", iteration);
		printf("got:      %llx ",(unsigned long long)(signed_result_v[0] >> 64));
		printf(" %llx \n",(unsigned long long)(signed_result_v[0]));
		printf("expected: %llx ",(unsigned long long)(signed_expected[iteration][0] >> 64));
		printf(" %llx \n",(unsigned long long)(signed_expected[iteration][0]));
		fflush(stdout);
      }
   }

   for (iteration = 0; iteration < ITERS ; iteration++ ) {
      unsigned_result_v = test_zero_extended_unsigned_load (iteration*2, (unsigned short*)buffer);
      if (unsigned_result_v[0] != unsigned_expected[iteration][0]) {
		mismatch++;
		printf("Unexpected results from unsigned load. i=%d \n", iteration);
		printf("got:      %llx ",(unsigned long long)(unsigned_result_v[0]>>64));
		printf(" %llx \n",(unsigned long long)(unsigned_result_v[0]));
		printf("expected: %llx ",(unsigned long long)(unsigned_expected[iteration][0]>>64));
		printf(" %llx \n",(unsigned long long)(unsigned_expected[iteration][0]));
		fflush(stdout);
      }
   }

   if (mismatch) {
      printf("%d mismatches. \n",mismatch);
      abort();
   }
   return 0;
}

