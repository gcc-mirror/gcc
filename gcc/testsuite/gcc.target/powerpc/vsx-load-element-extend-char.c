/*
   Test of vec_xl_sext and vec_xl_zext (load into rightmost
   vector element and zero/sign extend). */

/* { dg-do run { target power10_hw } } */
/* { dg-do compile { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -save-temps" } */

/* At the time of writing, the number of lxvrbx instructions is
   double what we expect because we are generating a 
   .constprop copy of the function.  */
/* { dg-final { scan-assembler-times {\mlxvrbx\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlbx\M} 0 } } */

#define NUM_VEC_ELEMS 16
#define ITERS 16

/*
Codegen at time of writing is a lxvrbx for both the
zero and sign extended tests.  The sign extension test
also uses mfvsr*d, extsb, mtvsrdd, vextsd2q.

0000000010000c90 <test_sign_extended_load>:
    10000c90:	1a 18 04 7c 	lxvrbx  vs0,r4,r3
    10000c94:	66 00 0b 7c 	mfvsrd  r11,vs0
    10000c98:	66 02 0a 7c 	mfvsrld r10,vs0
    10000c9c:	74 07 4a 7d 	extsb   r10,r10
    10000ca0:	67 53 40 7c 	mtvsrdd vs34,0,r10
    10000ca4:	02 16 5b 10 	vextsd2q v2,v2
    10000ca8:	20 00 80 4e 	blr

0000000010000cc0 <test_zero_extended_unsigned_load>:
    10000cc0:	1b 18 44 7c 	lxvrbx  vs34,r4,r3
    10000cc4:	20 00 80 4e 	blr
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
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000011},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000012},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000013},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000014},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000015},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000016},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000017},
	{ (__int128) 0x0000000000000000 << 64 | (__int128) 0x0000000000000018},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff89},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8a},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8b},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8c},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8d},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8e},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff8f},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffffffff80}
};

vector unsigned __int128 unsigned_expected[16] = {
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000011},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000012},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000013},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000014},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000015},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000016},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000017},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000018},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000089},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008a},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008b},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008c},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008d},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008e},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x000000000000008f},
	{ (unsigned __int128) 0x0000000000000000 << 64 | (unsigned __int128) 0x0000000000000080}
};

__attribute__ ((noinline))
vector signed __int128 test_sign_extended_load(int RA, signed char * RB) {
	return vec_xl_sext (RA, RB);
}

__attribute__ ((noinline))
vector unsigned __int128 test_zero_extended_unsigned_load(int RA, unsigned char * RB) {
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
      signed_result_v = test_sign_extended_load (iteration, (signed char*)buffer);
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
      unsigned_result_v = test_zero_extended_unsigned_load (iteration, (unsigned char*)buffer);
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

