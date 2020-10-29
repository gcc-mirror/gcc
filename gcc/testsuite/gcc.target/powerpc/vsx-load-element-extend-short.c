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

char initbuffer[64] = {
	0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
			0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x80,
	0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
			0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0x90,
	0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
			0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xa0,
	0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
			0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xb0
};

vector signed __int128 signed_expected[16] = {
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001211},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001312},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001413},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001514},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001615},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001716},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000001817},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8918},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8a89},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8b8a},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8c8b},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8d8c},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8e8d},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff8f8e},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffff808f},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000002180}
};

vector unsigned __int128 unsigned_expected[16] = {
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001211},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001312},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001413},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001514},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001615},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001716},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000001817},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008918},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008a89},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008b8a},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008c8b},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008d8c},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008e8d},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000008f8e},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x0000000000000808f},
	{ (unsigned __int128) 0x0000000000000000  << 64 | (unsigned __int128) 0x00000000000002180}
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
	   for (int k=0;k<64;k++) {
		   printf("%x ",initbuffer[k]);
		   if (k && (k+1)%16==0) printf("\n");
	   }
	   printf("signed_expected:\n");
	   for (int k=0;k<ITERS;k++) {
		printf("%llx ",signed_expected[iteration][0]>>64);
		printf(" %llx \n",signed_expected[iteration][0]&0xffffffffffffffff);
		   printf("\n");
	   }
	   printf("unsigned_expected:\n");
	   for (int k=0;k<ITERS;k++) {
		printf("%llx ",signed_expected[iteration][0]>>64);
		printf(" %llx \n",signed_expected[iteration][0]&0xffffffffffffffff);
		   printf("\n");
	   }
   }

   for (iteration = 0; iteration < ITERS ; iteration++ ) {
      signed_result_v = test_sign_extended_load (iteration, (signed short*)buffer);
      if (signed_result_v[0] != signed_expected[iteration][0] ) {
		mismatch++;
		printf("Unexpected results from signed load. i=%d \n", iteration);
		printf("got:      %llx ",signed_result_v[0]>>64);
		printf(" %llx \n",signed_result_v[0]&0xffffffffffffffff);
		printf("expected: %llx ",signed_expected[iteration][0]>>64);
		printf(" %llx \n",signed_expected[iteration][0]&0xffffffffffffffff);
		fflush(stdout);
      }
   }

   for (iteration = 0; iteration < ITERS ; iteration++ ) {
      unsigned_result_v = test_zero_extended_unsigned_load (iteration, (unsigned short*)buffer);
      if (unsigned_result_v[0] != unsigned_expected[iteration][0]) {
		mismatch++;
		printf("Unexpected results from unsigned load. i=%d \n", iteration);
		printf("got:      %llx ",unsigned_result_v[0]>>64);
		printf(" %llx \n",unsigned_result_v[0]&0xffffffffffffffff);
		printf("expected: %llx ",unsigned_expected[iteration][0]>>64);
		printf(" %llx \n",unsigned_expected[iteration][0]&0xffffffffffffffff);
		fflush(stdout);
      }
   }

   if (mismatch) {
      printf("%d mismatches. \n",mismatch);
      abort();
   }
   return 0;
}

