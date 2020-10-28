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
  vector signed char vsrc_a_char, vsrc_b_char;
  vector signed char vresult_char;
  vector signed char expected_vresult_char;

  vector unsigned char vsrc_a_uchar, vsrc_b_uchar, vsrc_c_uchar;
  vector unsigned char vresult_uchar;
  vector unsigned char expected_vresult_uchar;

  vector signed short vsrc_a_short, vsrc_b_short, vsrc_c_short;
  vector signed short vresult_short;
  vector signed short expected_vresult_short;

  vector unsigned short vsrc_a_ushort, vsrc_b_ushort, vsrc_c_ushort;
  vector unsigned short vresult_ushort;
  vector unsigned short expected_vresult_ushort;

  vector int vsrc_a_int, vsrc_b_int, vsrc_c_int;
  vector int vresult_int;
  vector int expected_vresult_int;

  vector unsigned int vsrc_a_uint, vsrc_b_uint, vsrc_c_uint;
  vector unsigned int vresult_uint;
  vector unsigned int expected_vresult_uint;

  vector long long int vsrc_a_ll, vsrc_b_ll, vsrc_c_ll;
  vector long long int vresult_ll;
  vector long long int expected_vresult_ll;

  vector unsigned long long int vsrc_a_ull,  vsrc_b_ull,  vsrc_c_ull;
  vector unsigned long long int vresult_ull;
  vector unsigned long long int expected_vresult_ull;

  vector float vresult_f;
  vector float expected_vresult_f;
  vector float vsrc_a_f, vsrc_b_f;

  vector double vsrc_a_d, vsrc_b_d;
  vector double vresult_d;
  vector double expected_vresult_d;
 
  /* Vector permx */
  vsrc_a_char = (vector signed char) { -1, 3, 5, 7, 9, 11, 13, 15,
                                       17, 19, 21, 23, 25, 27, 29 };
  vsrc_b_char = (vector signed char) { 2, -4, 6, 8, 10, 12, 14, 16,
				       18, 20, 22, 24, 26, 28, 30, 32 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x7, 0, 0x5, 0, 0x3, 0, 0x1,
					  0, 0x2, 0, 0x4, 0, 0x6, 0, 0x0 };
  vresult_char = (vector signed char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_char = (vector signed char) { -1, 15, -1, 11,
						 -1, 7, -1, 3,
						 -1, 5, -1, 9,
						 -1, 13, -1, -1 };
						 
  vresult_char = vec_permx (vsrc_a_char, vsrc_b_char, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_char,  expected_vresult_char)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_char, vsrc_b_char, vsrc_c_uchar)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_char[%d] = %d, expected_vresult_char[%d] = %d\n",
	     i, vresult_char[i], i, expected_vresult_char[i]);
#else
    abort();
#endif
  }

  vsrc_a_uchar = (vector unsigned char) { 1, 3, 5, 7, 9, 11, 13, 15,
					  17, 19, 21, 23, 25, 27, 29 };
  vsrc_b_uchar = (vector unsigned char) { 2, 4, 6, 8, 10, 12, 14, 16,
					  18, 20, 22, 24, 26, 28, 30, 32 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x7, 0, 0x5, 0, 0x3, 0, 0x1,
					  0, 0x2, 0, 0x4, 0, 0x6, 0, 0x0 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar = (vector unsigned char) { 1, 15, 1, 11,
						    1, 7, 1, 3,
						    1, 5, 1, 9,
						    1, 13, 1, 1 };
						 
  vresult_uchar = vec_permx (vsrc_a_uchar, vsrc_b_uchar, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_uchar,  expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_uchar, vsrc_b_uchar, vsrc_c_uchar)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = %d, expected_vresult_uchar[%d] = %d\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  vsrc_a_short = (vector signed short int) { 1, -3, 5, 7, 9, 11, 13, 15 };
  vsrc_b_short = (vector signed short int) { 2, 4, -6, 8, 10, 12, 14, 16 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x2, 0x3,
					  0x8, 0x9, 0x2, 0x3,
					  0x1E, 0x1F, 0x2, 0x3 };
  vresult_short = (vector signed short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_short = (vector signed short int) { 1, -3, 5, -3,
						       9, -3, 16, -3 };
						 
  vresult_short = vec_permx (vsrc_a_short, vsrc_b_short, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_short,  expected_vresult_short)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_short, vsrc_b_short, vsrc_c_uchar)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_short[%d] = %d, expected_vresult_short[%d] = %d\n",
	     i, vresult_short[i], i, expected_vresult_short[i]);
#else
    abort();
#endif
  }

  vsrc_a_ushort = (vector unsigned short int) { 1, 3, 5, 7, 9, 11, 13, 15 };
  vsrc_b_ushort = (vector unsigned short int) { 2, 4, 6, 8, 10, 12, 14, 16 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x2, 0x3,
					  0x8, 0x9, 0x2, 0x3,
					  0x1E, 0x1F, 0x2, 0x3 };
  vresult_ushort = (vector unsigned short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ushort = (vector unsigned short int) { 1, 3, 5, 3,
							  9, 3, 16, 3 };
						 
  vresult_ushort = vec_permx (vsrc_a_ushort, vsrc_b_ushort, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_ushort,  expected_vresult_ushort)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_ushort, vsrc_b_ushort, vsrc_c_uchar)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_ushort[%d] = %d, expected_vresult_ushort[%d] = %d\n",
	     i, vresult_ushort[i], i, expected_vresult_ushort[i]);
#else
    abort();
#endif
  }

  vsrc_a_int = (vector signed int) { 1, -3, 5, 7 };
  vsrc_b_int = (vector signed int) { 2, 4, -6, 8 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x18, 0x19, 0x1A, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_int = (vector signed int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector signed int) { 1, -3, -6, 8 };
						 
  vresult_int = vec_permx (vsrc_a_int, vsrc_b_int, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_int, vsrc_b_int, vsrc_c_uchar)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  vsrc_a_uint = (vector unsigned int) { 1, 3, 5, 7 };
  vsrc_b_uint = (vector unsigned int) { 10, 12, 14, 16 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x18, 0x19, 0x1A, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_uint = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_uint = (vector unsigned int) { 1, 3, 14, 16 };
						 
  vresult_uint = vec_permx (vsrc_a_uint, vsrc_b_uint, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_uint,  expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_uint, vsrc_b_uint, vsrc_c_uchar)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_uint[%d] = %d, expected_vresult_uint[%d] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }

  vsrc_a_ll = (vector signed long long int) { 1, -3 };
  vsrc_b_ll = (vector signed long long int) { 2, -4 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x18, 0x19, 0x1A, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_ll = (vector signed long long int) { 0, 0};
  expected_vresult_ll = (vector signed long long int) { 1, -4 };
						 
  vresult_ll = vec_permx (vsrc_a_ll, vsrc_b_ll, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_ll,  expected_vresult_ll)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_ll, vsrc_b_ll, vsrc_c_uchar)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ll[%d] = %lld, expected_vresult_ll[%d] = %lld\n",
	     i, vresult_ll[i], i, expected_vresult_ll[i]);
#else
    abort();
#endif
  }

  vsrc_a_ull = (vector unsigned long long int) { 1, 3 };
  vsrc_b_ull = (vector unsigned long long int) { 10, 12 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x18, 0x19, 0x1A, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_ull = (vector unsigned long long int) { 0, 0 };
  expected_vresult_ull = (vector unsigned long long int) { 1, 12 };
						 
  vresult_ull = vec_permx (vsrc_a_ull, vsrc_b_ull, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_ull,  expected_vresult_ull)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_ull, vsrc_b_ull, vsrc_c_uchar)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ull[%d] = %d, expected_vresult_ull[%d] = %d\n",
	     i, vresult_ull[i], i, expected_vresult_ull[i]);
#else
    abort();
#endif
  }

  vsrc_a_f = (vector float) { -3.0, 5.0, 7.0, 9.0 };
  vsrc_b_f = (vector float) { 2.0,  4.0, 6.0, 8.0  };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x18, 0x19, 0x1A, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_f = (vector float) { 0.0, 0.0, 0.0, 0.0 };
  expected_vresult_f = (vector float) { -3.0, 5.0, 6.0, 8.0 };
						 
  vresult_f = vec_permx (vsrc_a_f, vsrc_b_f, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_f,  expected_vresult_f)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_f, vsrc_b_f, vsrc_c_uchar)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_f[%d] = %f, expected_vresult_f[%d] = %f\n",
	     i, vresult_f[i], i, expected_vresult_f[i]);
#else
    abort();
#endif
  }

  vsrc_a_d = (vector double) { 1.0, -3.0 };
  vsrc_b_d = (vector double) { 2.0, -4.0 };
  vsrc_c_uchar = (vector unsigned char) { 0x0, 0x1, 0x2, 0x3,
					  0x4, 0x5, 0x6, 0x7,
					  0x1A, 0x1B, 0x1C, 0x1B,
					  0x1C, 0x1D, 0x1E, 0x1F };
  vresult_d = (vector double) { 0.0, 0.0 };
  expected_vresult_d = (vector double) { 1.0, -4.0 };
						 
  vresult_d = vec_permx (vsrc_a_d, vsrc_b_d, vsrc_c_uchar, 0);

  if (!vec_all_eq (vresult_d,  expected_vresult_d)) {
#if DEBUG
    printf("ERROR, vec_permx (vsrc_a_d, vsrc_b_d, vsrc_c_uchar)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_d[%d] = %f, expected_vresult_d[%d] = %f\n",
	     i, vresult_d[i], i, expected_vresult_d[i]);
#else
    abort();
#endif
  }

  return 0;
}

/* { dg-final { scan-assembler-times {\mxxpermx\M} 10 } } */


