/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */
#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

extern void abort (void);

int
main (int argc, char *argv [])
{
  int i;

  vector signed char vresult_char;
  vector signed char expected_vresult_char;
  vector signed char src_va_char;
  vector signed char src_vb_char;

  vector unsigned char vresult_uchar;
  vector unsigned char expected_vresult_uchar;
  vector unsigned char src_va_uchar;
  vector unsigned char src_vb_uchar;

  vector short int vresult_sh;
  vector short int expected_vresult_sh;
  vector short int src_va_sh;
  vector short int src_vb_sh;

  vector short unsigned int vresult_ush;
  vector short unsigned int expected_vresult_ush;
  vector short unsigned int src_va_ush;
  vector short unsigned int src_vb_ush;

  vector int vresult_int;
  vector int expected_vresult_int;
  vector int src_va_int;
  vector int src_vb_int;
  int src_a_int;

  vector unsigned int vresult_uint;
  vector unsigned int expected_vresult_uint;
  vector unsigned int src_va_uint;
  vector unsigned int src_vb_uint;
  unsigned int src_a_uint;

  vector long long int vresult_llint;
  vector long long int expected_vresult_llint;
  vector long long int src_va_llint;
  vector long long int src_vb_llint;
  long long int src_a_llint;

  vector unsigned long long int vresult_ullint;
  vector unsigned long long int expected_vresult_ullint;
  vector unsigned long long int src_va_ullint;
  vector unsigned long long int src_vb_ullint;
  unsigned int long long src_a_ullint;

  /* Vector shift double left */
  src_va_char = (vector signed char) { 0, 2, 4, 6, 8, 10, 12, 14,
				       16, 18, 20, 22, 24, 26, 28, 30 }; 
  src_vb_char = (vector signed char) { 10, 20, 30, 40, 50, 60, 70, 80, 90,
					100, 110, 120, 130, 140, 150, 160 };
  vresult_char = (vector signed char) { 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_char = (vector signed char) { 80, 0, 1, 2, 3, 4, 5, 6, 7,
						 8, 9, 10, 11, 12, 13, 14 }; 
						 
  vresult_char = vec_sldb (src_va_char, src_vb_char, 7);

  if (!vec_all_eq (vresult_char,  expected_vresult_char)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_char_, src_vb_char, 7)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_char[%d] = %d, expected_vresult_char[%d] = %d\n",
	     i, vresult_char[i], i, expected_vresult_char[i]);
#else
    abort();
#endif
  }

  src_va_uchar = (vector unsigned char) { 0, 2, 4, 6, 8, 10, 12, 14,
					  16, 18, 20, 22, 24, 26, 28, 30 }; 
  src_vb_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar = (vector unsigned char) { 0, 0, 1, 2, 3, 4, 5, 6, 7,
						    8, 9, 10, 11, 12, 13, 14 };
						 
  vresult_uchar = vec_sldb (src_va_uchar, src_vb_uchar, 7);

  if (!vec_all_eq (vresult_uchar,  expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_uchar_, src_vb_uchar, 7)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = %d, expected_vresult_uchar[%d] = %d\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  src_va_sh = (vector short int) { 0, 2, 4, 6, 8, 10, 12, 14 };
  src_vb_sh = (vector short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  vresult_sh = (vector short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector short int) { 0, 2*128, 4*128, 6*128,
					     8*128, 10*128, 12*128, 14*128 }; 
						 
  vresult_sh = vec_sldb (src_va_sh, src_vb_sh, 7);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_sh_, src_vb_sh, 7)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  src_va_ush = (vector short unsigned int) { 0, 2, 4, 6, 8, 10, 12, 14 };
  src_vb_ush = (vector short unsigned int) { 10, 20, 30, 40, 50, 60, 70, 80 };
  vresult_ush = (vector short unsigned int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ush = (vector short unsigned int) { 0, 2*128, 4*128, 6*128,
						       8*128, 10*128, 12*128,
						       14*128 }; 
						 
  vresult_ush = vec_sldb (src_va_ush, src_vb_ush, 7);

  if (!vec_all_eq (vresult_ush,  expected_vresult_ush)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_ush_, src_vb_ush, 7)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_ush[%d] = %d, expected_vresult_ush[%d] = %d\n",
	     i, vresult_ush[i], i, expected_vresult_ush[i]);
#else
    abort();
#endif
  }

  src_va_int = (vector signed int) { 0, 2, 3, 1 };
  src_vb_int = (vector signed int) { 0, 0, 0, 0 };
  vresult_int = (vector signed int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector signed int) { 0, 2*128, 3*128, 1*128 }; 
						 
  vresult_int = vec_sldb (src_va_int, src_vb_int, 7);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_int_, src_vb_int, 7)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  src_va_uint = (vector unsigned int) { 0, 2, 4, 6 };
  src_vb_uint = (vector unsigned int) { 10, 20, 30, 40 };
  vresult_uint = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_uint = (vector unsigned int) { 0, 2*128, 4*128, 6*128 }; 
						 
  vresult_uint = vec_sldb (src_va_uint, src_vb_uint, 7);

  if (!vec_all_eq (vresult_uint,  expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_uint_, src_vb_uint, 7)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_uint[%d] = %d, expected_vresult_uint[%d] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }

  src_va_llint = (vector signed long long int) { 5, 6 };
  src_vb_llint = (vector signed long long int) { 0, 0 };
  vresult_llint = (vector signed long long int) { 0, 0 };
  expected_vresult_llint = (vector signed long long int) { 5*128, 6*128 }; 
						 
  vresult_llint = vec_sldb (src_va_llint, src_vb_llint, 7);

  if (!vec_all_eq (vresult_llint,  expected_vresult_llint)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_llint_, src_vb_llint, 7)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_llint[%d] = %d, expected_vresult_llint[%d] = %d\n",
	     i, vresult_llint[i], i, expected_vresult_llint[i]);
#else
    abort();
#endif
  }

  src_va_ullint = (vector unsigned long long int) { 54, 26 };
  src_vb_ullint = (vector unsigned long long int) { 10, 20 };
  vresult_ullint = (vector unsigned long long int) { 0, 0 };
  expected_vresult_ullint = (vector unsigned long long int) { 54*128,
							      26*128 }; 
						 
  vresult_ullint = vec_sldb (src_va_ullint, src_vb_ullint, 7);

  if (!vec_all_eq (vresult_ullint,  expected_vresult_ullint)) {
#if DEBUG
    printf("ERROR, vec_sldb (src_va_ullint_, src_vb_ullint, 7)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ullint[%d] = %d, expected_vresult_ullint[%d] = %d\n",
	     i, vresult_ullint[i], i, expected_vresult_ullint[i]);
#else
    abort();
#endif
  }

  /* Vector shift double right */
  src_va_char = (vector signed char) { 0, 2, 4, 6, 8, 10, 12, 14,
				       16, 18, 20, 22, 24, 26, 28, 30 }; 
  src_vb_char = (vector signed char) { 10, 12, 14, 16, 18, 20, 22, 24, 26,
					28, 30, 32, 34, 36, 38, 40 };
  vresult_char = (vector signed char) { 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_char = (vector signed char) { 24, 28, 32, 36, 40, 44, 48,
						 52, 56, 60, 64, 68, 72, 76,
						 80, 0 }; 
						 
  vresult_char = vec_srdb (src_va_char, src_vb_char, 7);

  if (!vec_all_eq (vresult_char,  expected_vresult_char)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_char_, src_vb_char, 7)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_char[%d] = %d, expected_vresult_char[%d] = %d\n",
	     i, vresult_char[i], i, expected_vresult_char[i]);
#else
    abort();
#endif
  }

  src_va_uchar = (vector unsigned char) { 100, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  src_vb_uchar = (vector unsigned char) { 0, 2, 4, 6, 8, 10, 12, 14,
					  16, 18, 20, 22, 24, 26, 28, 30 }; 
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar = (vector unsigned char) { 4, 8, 12, 16, 20, 24, 28,
						    32, 36, 40, 44, 48, 52,
						    56, 60, 200 };
						 
  vresult_uchar = vec_srdb (src_va_uchar, src_vb_uchar, 7);

  if (!vec_all_eq (vresult_uchar,  expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_uchar_, src_vb_uchar, 7)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = %d, expected_vresult_uchar[%d] = %d\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  src_va_sh = (vector short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  src_vb_sh = (vector short int) { 0, 2*128, 4*128, 6*128,
					     8*128, 10*128, 12*128, 14*128 };
  vresult_sh = (vector short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector short int) { 0, 2, 4, 6, 8, 10, 12, 14 }; 
						 
  vresult_sh = vec_srdb (src_va_sh, src_vb_sh, 7);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_sh_, src_vb_sh, 7)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  src_va_ush = (vector short unsigned int) { 0, 20, 30, 40, 50, 60, 70, 80 };
  src_vb_ush = (vector short unsigned int) { 0, 2*128, 4*128, 6*128,
					     8*128, 10*128, 12*128, 14*128 };
  vresult_ush = (vector short unsigned int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ush = (vector short unsigned int) { 0, 2, 4, 6, 8, 10,
						       12, 14 }; 
						 
  vresult_ush = vec_srdb (src_va_ush, src_vb_ush, 7);

  if (!vec_all_eq (vresult_ush,  expected_vresult_ush)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_ush_, src_vb_ush, 7)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_ush[%d] = %d, expected_vresult_ush[%d] = %d\n",
	     i, vresult_ush[i], i, expected_vresult_ush[i]);
#else
    abort();
#endif
  }

  src_va_int = (vector signed int) { 0, 0, 0, 0 };
  src_vb_int = (vector signed int) { 0, 2*128, 3*128, 1*128 };
  vresult_int = (vector signed int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector signed int) { 0, 2, 3, 1  }; 
						 
  vresult_int = vec_srdb (src_va_int, src_vb_int, 7);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_int_, src_vb_int, 7)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  src_va_uint = (vector unsigned int) { 0, 20, 30, 40 };
  src_vb_uint = (vector unsigned int) { 128, 2*128, 4*128, 6*128 };
  vresult_uint = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_uint = (vector unsigned int) { 1, 2, 4, 6 }; 
						 
  vresult_uint = vec_srdb (src_va_uint, src_vb_uint, 7);

  if (!vec_all_eq (vresult_uint,  expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_uint_, src_vb_uint, 7)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_uint[%d] = %d, expected_vresult_uint[%d] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }

  src_va_llint = (vector signed long long int) { 0, 0 };
  src_vb_llint = (vector signed long long int) { 5*128, 6*128 };
  vresult_llint = (vector signed long long int) { 0, 0 };
  expected_vresult_llint = (vector signed long long int) { 5, 6 }; 
						 
  vresult_llint = vec_srdb (src_va_llint, src_vb_llint, 7);

  if (!vec_all_eq (vresult_llint,  expected_vresult_llint)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_llint_, src_vb_llint, 7)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_llint[%d] = %d, expected_vresult_llint[%d] = %d\n",
	     i, vresult_llint[i], i, expected_vresult_llint[i]);
#else
    abort();
#endif
  }

  src_va_ullint = (vector unsigned long long int) { 0, 0 };
  src_vb_ullint = (vector unsigned long long int) { 54*128, 26*128 };
  vresult_ullint = (vector unsigned long long int) { 0, 0 };
  expected_vresult_ullint = (vector unsigned long long int) { 54, 26 }; 

  vresult_ullint = vec_srdb (src_va_ullint, src_vb_ullint, 7);

  if (!vec_all_eq (vresult_ullint,  expected_vresult_ullint)) {
#if DEBUG
    printf("ERROR, vec_srdb (src_va_ullint_, src_vb_ullint, 7)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ullint[%d] = %d, expected_vresult_ullint[%d] = %d\n",
	     i, vresult_ullint[i], i, expected_vresult_ullint[i]);
#else
    abort();
#endif
  }

  return 0;
}

/* { dg-final { scan-assembler-times {\mvsldbi\M} 8 } } */
/* { dg-final { scan-assembler-times {\mvsrdbi\M} 8 } } */
