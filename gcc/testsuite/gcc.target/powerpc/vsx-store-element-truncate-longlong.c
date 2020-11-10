/*
   Test of vec_xst_trunc (truncate and store rightmost vector element) */

/* { dg-do run { target power10_hw } } */
/* { dg-do compile { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */

/* Deliberately set optization to zero for this test to confirm
   the stxvr*x instruction is generated. At higher optimization levels
   the instruction we are looking for is sometimes replaced by other
   store instructions. */
/* { dg-options "-mdejagnu-cpu=power10 -O0 -save-temps" } */

/* { dg-final { scan-assembler-times {\mstxvrdx\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstwx\M} 0 } } */

#include <altivec.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

vector signed __int128 store_this_s[4] = {
{ (__int128) 0x7000000000000000 << 64 | (__int128) 0x123456789abcdef8ULL},
{ (__int128) 0x8000000000000000 << 64 | (__int128) 0xfedcba9876543217ULL},
{ (__int128) 0x1000000000000000 << 64 | (__int128) 0xccccccccccccccccULL},
{ (__int128) 0xf000000000000000 << 64 | (__int128) 0xaaaaaaaaaaaaaaaaULL}
};

vector unsigned __int128 store_this_us[4] = {
{ (unsigned __int128) 0x7000000000000000 << 64 | (unsigned __int128) 0x123456789abcdef8ULL},
{ (unsigned __int128) 0x8000000000000000 << 64 | (unsigned __int128) 0xfedcba9876543217ULL},
{ (unsigned __int128) 0x1000000000000000 << 64 | (unsigned __int128) 0xeeeeeeeeeeeeeeeeULL},
{ (unsigned __int128) 0xf000000000000000 << 64 | (unsigned __int128) 0x5555555555555555ULL}
};

#define NUM_VEC_ELEMS 2

vector signed long long signed_expected[5] = {
	{ 0x123456789abcdef8,                0x0},
	{ 0x7654321700000000,         0xfedcba98},
	{ 0x0000000000000000, 0xcccccccccccccccc},
	{ 0x0000000000000000, 0xaaaaaaaa00000000}  /*note that some data written into the next word */
};
vector unsigned long long unsigned_expected[5] = {
	{ 0x123456789abcdef8,                0x0},
	{ 0x7654321700000000,         0xfedcba98},
	{ 0x0000000000000000, 0xeeeeeeeeeeeeeeee},
	{ 0x0000000000000000, 0x5555555500000000}
};

unsigned long long rawbuffer[32];
signed long long * vsbuffer = (long long *)rawbuffer;
unsigned long long * vubuffer = (unsigned long long *)rawbuffer;

void reset_buffer() {
	memset (&rawbuffer,0,sizeof(rawbuffer));
}

#define PRINT_VEC(V) \
   for (int j=0;j<NUM_VEC_ELEMS;j++) {	printf ("(0x%lx) ", V[j] ); }

void test_signed_store(vector signed __int128 myvec, int offset, signed long long * store_data ) {
	vec_xst_trunc (myvec, offset, store_data);
}

void test_unsigned_store(vector unsigned __int128 myvec, int offset, unsigned long long * store_data )   {
	vec_xst_trunc (myvec, offset, store_data);
}

int main (int argc, char *argv [])
{
   int i;
   int memcmpresult;
   int mismatch=0;
   int verbose=0;

#if VERBOSE
   verbose=1;
   printf("%s %s\n", __DATE__, __TIME__);
#endif

   if (verbose) {
      printf("expected results from signed tests:\n");
      for (i = 0; i < 4 ; i++ ) {
	 PRINT_VEC(signed_expected[i]);
	 printf("\n");
      }
   }

   for (i = 0; i < 4 ; i++ ) {
      reset_buffer();
      test_signed_store (store_this_s[i], 4*i, vsbuffer);
      memcmpresult = memcmp(rawbuffer,&signed_expected[i],sizeof(vector long long));
      if (memcmpresult) {
	 printf("mismatch signed buffer, i %d (memcmpresult:%d) \n",i,memcmpresult);
	 mismatch++;
	 if (verbose) {
	    printf("results: ");
	    PRINT_VEC(vsbuffer);
	    printf("\n");
	 }
      }
   }

   for (i = 0; i < 4 ; i++ ) {
      reset_buffer();
      test_unsigned_store (store_this_us[i], 4*i, vubuffer);
      memcmpresult = memcmp(rawbuffer,&unsigned_expected[i],sizeof(vector long long));
      if (memcmpresult) {
	 printf("mismatch unsigned buffer, i %d (memcmpresult:%d) \n",i,memcmpresult);
	 mismatch++;
	 if (verbose) {
	    printf("results :");
	    PRINT_VEC(vubuffer);
	    printf("\n");
	 }
      }
   }

   if (mismatch) {
      printf("%d mismatches. \n",mismatch);
      abort();
   }
   return 0;
}

