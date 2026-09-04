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
   load instructions. */
/* { dg-options "-mdejagnu-cpu=power10 -O0 -save-temps" } */

/* { dg-final { scan-assembler-times {\mlxvrwx\M} 2 } } */

#define NUM_VEC_ELEMS 4
#define ITERS 16

/*
Codegen at time of writing is a single lxvrwx for the zero
extended test, and a lwax,mtvsrdd,vextsd2q for the sign
extended test.

0000000010000c90 <test_sign_extended_load>:
    10000c90:	aa 1a 24 7d 	lwax    r9,r4,r3
    10000c94:	67 4b 40 7c 	mtvsrdd vs34,0,r9
    10000c98:	02 16 5b 10 	vextsd2q v2,v2
    10000c9c:	20 00 80 4e 	blr

0000000010000cb0 <test_zero_extended_unsigned_load>:
    10000cb0:	9b 18 44 7c 	lxvrwx  vs34,r4,r3
    10000cb4:	20 00 80 4e 	blr
*/

#include <altivec.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

long long buffer[8];
unsigned long verbose=0;

int initbuffer[16] = {
	0x11121314, 0x15161718,
			0x898a8b8c, 0x8d8e8f80,
	0x21222324, 0x25262728,
			0x999a9b9c, 0x9d9e9f90,
	0x31323334, 0x35363738,
			0xa9aaabac, 0xadaeafa0,
	0x41424344, 0x45464748,
			0xb9babbbc, 0xbdbebfb0
};

vector signed __int128 signed_expected[16] = {
        { (__int128) 0x0 << 64 | (__int128) 0x11121314},
        { (__int128) 0x0 << 64 | (__int128) 0x15161718},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffff898a8b8c},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffff8d8e8f80},
        { (__int128) 0x0 << 64 | (__int128) 0x21222324},
        { (__int128) 0x0 << 64 | (__int128) 0x25262728},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffff999a9b9c},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffff9d9e9f90},
        { (__int128) 0x0 << 64 | (__int128) 0x31323334},
        { (__int128) 0x0 << 64 | (__int128) 0x35363738},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffa9aaabac},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffadaeafa0},
        { (__int128) 0x0 << 64 | (__int128) 0x41424344},
        { (__int128) 0x0 << 64 | (__int128) 0x45464748},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffb9babbbc},
        { (__int128) 0xffffffffffffffff << 64 | (__int128) 0xffffffffbdbebfb0}
};

vector unsigned __int128 unsigned_expected[16] = {
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x11121314},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x15161718},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x898a8b8c},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x8d8e8f80},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x21222324},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x25262728},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x999a9b9c},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x9d9e9f90},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x31323334},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x35363738},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xa9aaabac},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xadaeafa0},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x41424344},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0x45464748},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xb9babbbc},
        { (unsigned __int128) 0x0 << 64 | (unsigned __int128) 0xbdbebfb0}
};

__attribute__ ((noinline))
vector signed __int128 test_sign_extended_load(int RA, signed int * RB) {
	return vec_xl_sext (RA, RB);
}

__attribute__ ((noinline))
vector unsigned __int128 test_zero_extended_unsigned_load(int RA, unsigned int * RB) {
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
	   for (int k=0;k<16;k++) {
		   printf("%8x ",initbuffer[k]);
		   if (k && (k+1)%2==0) printf("\n");
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
      signed_result_v = test_sign_extended_load (iteration*4, (signed int*)buffer);
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
      unsigned_result_v = test_zero_extended_unsigned_load (iteration*4, (unsigned int*)buffer);
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

