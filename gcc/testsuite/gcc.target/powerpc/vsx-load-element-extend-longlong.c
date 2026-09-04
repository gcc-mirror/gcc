/*
   Test of vec_xl_sext and vec_xl_zext (load into rightmost
   vector element and zero/sign extend). */

/* { dg-do run { target power10_hw } } */
/* { dg-do compile { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -save-temps" } */

/* At time of writing, we also geenerate a .constrprop copy
   of the function, so our instruction hit count is
   twice of what we would otherwise expect.  */
/* { dg-final { scan-assembler-times {\mlxvrdx\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlvdx\M} 0 } } */

#define NUM_VEC_ELEMS 2
#define ITERS 8

/*
Codegen at time of writing uses lxvrdx for both sign and
zero extend tests. The sign extended test also uses
mfvsr*d, mtvsrdd, vextsd2q.

0000000010000c90 <test_sign_extended_load>:
    10000c90:	da 18 04 7c 	lxvrdx  vs0,r4,r3
    10000c94:	66 00 0b 7c 	mfvsrd  r11,vs0
    10000c98:	66 02 0a 7c 	mfvsrld r10,vs0
    10000c9c:	67 53 40 7c 	mtvsrdd vs34,0,r10
    10000ca0:	02 16 5b 10 	vextsd2q v2,v2
    10000ca4:	20 00 80 4e 	blr

0000000010000cc0 <test_zero_extended_unsigned_load>:
    10000cc0:	db 18 44 7c 	lxvrdx  vs34,r4,r3
    10000cc4:	20 00 80 4e 	blr
*/

#include <altivec.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

long long buffer[8];
unsigned long verbose=0;

long long initbuffer[8] = {
	0x1112131415161718,
	0x898a8b8c8d8e8f80,
	0x2122232425262728,
	0x999a9b9c9d9e9f90,
	0x3132333435363738,
	0xa9aaabacadaeafa0,
	0x4142434445464748,
	0xb9babbbcbdbebfb0
};

vector signed __int128 signed_expected[8] = {
	{ (__int128) 0x0 << 64 | (__int128) 0x1112131415161718},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0x898a8b8c8d8e8f80},
	{ (__int128) 0x0 << 64 | (__int128) 0x2122232425262728},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0x999a9b9c9d9e9f90},
	{ (__int128) 0x0 << 64 | (__int128) 0x3132333435363738},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xa9aaabacadaeafa0},
	{ (__int128) 0x0 << 64 | (__int128) 0x4142434445464748},
	{ (__int128) 0xffffffffffffffff << 64 | (__int128) 0xb9babbbcbdbebfb0}
};

vector unsigned __int128 unsigned_expected[8] = {
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x1112131415161718},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x898a8b8c8d8e8f80},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x2122232425262728},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x999a9b9c9d9e9f90},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x3132333435363738},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xa9aaabacadaeafa0},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x4142434445464748},
	{ (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xb9babbbcbdbebfb0}
};

__attribute__ ((noinline))
vector signed __int128 test_sign_extended_load(int RA, signed long long * RB) {
	return vec_xl_sext (RA, RB);
}

__attribute__ ((noinline))
vector unsigned __int128 test_zero_extended_unsigned_load(int RA, unsigned long long * RB) {
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
	   for (int k=0;k<8;k++) {
		   printf("%llx ",initbuffer[k]);
		   printf("\n");
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
      signed_result_v = test_sign_extended_load (iteration*8, (signed long long*)buffer);
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
      unsigned_result_v = test_zero_extended_unsigned_load (iteration*8, (unsigned long long*)buffer);
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

