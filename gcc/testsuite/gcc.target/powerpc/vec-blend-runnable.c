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
 
  /* Vector blend */
  vsrc_c_uchar = (vector unsigned char) { 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80,
					  0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80 };

  vsrc_a_char = (vector signed char) { -1, 3, 5, 7, 9, 11, 13, 15,
                                       17, 19, 21, 23, 25, 27, 29 };
  vsrc_b_char = (vector signed char) { 2, -4, 6, 8, 10, 12, 14, 16,
				       18, 20, 22, 24, 26, 28, 30, 32 };
  vsrc_c_uchar = (vector unsigned char) { 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80,
					  0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80 };
  vresult_char = (vector signed char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_char = (vector signed char) { -1, -4, 5, 8,
						 9, 12, 13, 16,
						 17, 20, 21, 24,
						 25, 28, 29, 32 };
						 
  vresult_char = vec_blendv (vsrc_a_char, vsrc_b_char, vsrc_c_uchar);

  if (!vec_all_eq (vresult_char,  expected_vresult_char)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_char, vsrc_b_char, vsrc_c_uchar)\n");
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
  vsrc_c_uchar = (vector unsigned char) { 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80,
					  0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar = (vector unsigned char) { 1, 4, 5, 8,
						    9, 12, 13, 16,
						    17, 20, 21, 24,
						    25, 28, 29, 32 };
						 
  vresult_uchar = vec_blendv (vsrc_a_uchar, vsrc_b_uchar, vsrc_c_uchar);

  if (!vec_all_eq (vresult_uchar,  expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_uchar, vsrc_b_uchar, vsrc_c_uchar)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = %d, expected_vresult_uchar[%d] = %d\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  vsrc_a_short = (vector signed short) { -1, 3, 5, 7, 9, 11, 13, 15 };
  vsrc_b_short = (vector signed short) { 2, -4, 6, 8, 10, 12, 14, 16 };
  vsrc_c_ushort = (vector unsigned short) { 0, 0x8000, 0, 0x8000,
					    0, 0x8000, 0, 0x8000 };
  vresult_short = (vector signed short) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_short = (vector signed short) { -1, -4, 5, 8,
						   9, 12, 13, 16 };

  vresult_short = vec_blendv (vsrc_a_short, vsrc_b_short, vsrc_c_ushort);

  if (!vec_all_eq (vresult_short,  expected_vresult_short)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_short, vsrc_b_short, vsrc_c_ushort)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_short[%d] = %d, expected_vresult_short[%d] = %d\n",
	     i, vresult_short[i], i, expected_vresult_short[i]);
#else
    abort();
#endif
  }

  vsrc_a_ushort = (vector unsigned short) { 1, 3, 5, 7, 9, 11, 13, 15 };
  vsrc_b_ushort = (vector unsigned short) { 2, 4, 6, 8, 10, 12, 14, 16 };
  vsrc_c_ushort = (vector unsigned short) { 0, 0x8000, 0, 0x8000,
					    0, 0x8000, 0, 0x8000 };
  vresult_ushort = (vector unsigned short) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ushort = (vector unsigned short) { 1, 4, 5, 8,
						      9, 12, 13, 16 };
						 
  vresult_ushort = vec_blendv (vsrc_a_ushort, vsrc_b_ushort, vsrc_c_ushort);

  if (!vec_all_eq (vresult_ushort,  expected_vresult_ushort)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_ushort, vsrc_b_ushort, "
	   "vsrc_c_ushort)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_ushort[%d] = %d, expected_vresult_ushort[%d] = %d\n",
	     i, vresult_ushort[i], i, expected_vresult_ushort[i]);
#else
    abort();
#endif
  }

  vsrc_a_int = (vector signed int) { -1, -3, -5, -7 };
  vsrc_b_int = (vector signed int) { 2, 4, 6, 8 };
  vsrc_c_uint = (vector unsigned int) { 0, 0x80000000, 0, 0x80000000};
  vresult_int = (vector signed int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector signed int) { -1, 4, -5, 8 };
						 
  vresult_int = vec_blendv (vsrc_a_int, vsrc_b_int, vsrc_c_uint);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_int, vsrc_b_int, vsrc_c_uint)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  vsrc_a_uint = (vector unsigned int) { 1, 3, 5, 7 };
  vsrc_b_uint = (vector unsigned int) { 2, 4, 6, 8 };
  vsrc_c_uint = (vector unsigned int) { 0, 0x80000000, 0, 0x80000000 };
  vresult_uint = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_uint = (vector unsigned int) { 1, 4, 5, 8 };
						 
  vresult_uint = vec_blendv (vsrc_a_uint, vsrc_b_uint, vsrc_c_uint);

  if (!vec_all_eq (vresult_uint,  expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_uint, vsrc_b_uint, vsrc_c_uint)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_uint[%d] = %d, expected_vresult_uint[%d] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }

  vsrc_a_ll = (vector signed long long int) { -1, -3 };
  vsrc_b_ll = (vector signed long long int) { 2, 4,  };
  vsrc_c_ull = (vector unsigned long long int) { 0, 0x8000000000000000ULL };
  vresult_ll = (vector signed long long int) { 0, 0 };
  expected_vresult_ll = (vector signed long long int) { -1, 4 };
						 
  vresult_ll = vec_blendv (vsrc_a_ll, vsrc_b_ll, vsrc_c_ull);

  if (!vec_all_eq (vresult_ll,  expected_vresult_ll)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_ll, vsrc_b_ll, vsrc_c_ull)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ll[%d] = %d, expected_vresult_ll[%d] = %d\n",
	     i, vresult_ll[i], i, expected_vresult_ll[i]);
#else
    abort();
#endif
  }

  vsrc_a_ull = (vector unsigned long long) { 1, 3 };
  vsrc_b_ull = (vector unsigned long long) { 2, 4 };
  vsrc_c_ull = (vector unsigned long long int) { 0, 0x8000000000000000ULL };
  vresult_ull = (vector unsigned long long) { 0, 0 };
  expected_vresult_ull = (vector unsigned long long) { 1, 4 };
						 
  vresult_ull = vec_blendv (vsrc_a_ull, vsrc_b_ull, vsrc_c_ull);

  if (!vec_all_eq (vresult_ull,  expected_vresult_ull)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_ull, vsrc_b_ull, vsrc_c_ull)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ull[%d] = %d, expected_vresult_ull[%d] = %d\n",
	     i, vresult_ull[i], i, expected_vresult_ull[i]);
#else
    abort();
#endif
  }

  vsrc_a_f = (vector float) { -1.0, -3.0, -5.0, -7.0 };
  vsrc_b_f = (vector float) { 2.0, 4.0, 6.0, 8.0 };
  vsrc_c_uint = (vector unsigned int) { 0, 0x80000000, 0, 0x80000000};
  vresult_f = (vector float) { 0, 0, 0, 0 };
  expected_vresult_f = (vector float) { -1, 4, -5, 8 };
						 
  vresult_f = vec_blendv (vsrc_a_f, vsrc_b_f, vsrc_c_uint);

  if (!vec_all_eq (vresult_f,  expected_vresult_f)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_f, vsrc_b_f, vsrc_c_uint)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_f[%d] = %d, expected_vresult_f[%d] = %d\n",
	     i, vresult_f[i], i, expected_vresult_f[i]);
#else
    abort();
#endif
  }

  vsrc_a_d = (vector double) { -1.0, -3.0 };
  vsrc_b_d = (vector double) { 2.0, 4.0 };
  vsrc_c_ull = (vector unsigned long long int) { 0, 0x8000000000000000ULL };
  vresult_d = (vector double) { 0, 0 };
  expected_vresult_d = (vector double) { -1, 4 };
						 
  vresult_d = vec_blendv (vsrc_a_d, vsrc_b_d, vsrc_c_ull);

  if (!vec_all_eq (vresult_d,  expected_vresult_d)) {
#if DEBUG
    printf("ERROR, vec_blendv (vsrc_a_d, vsrc_b_d, vsrc_c_ull)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_d[%d] = %d, expected_vresult_d[%d] = %d\n",
	     i, vresult_d[i], i, expected_vresult_d[i]);
#else
    abort();
#endif
  }

  return 0;
}

/* { dg-final { scan-assembler-times {\mxxblendvb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxblendvh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxblendvw\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxblendvd\M} 3 } } */
