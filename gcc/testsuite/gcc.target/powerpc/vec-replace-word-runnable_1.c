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
  unsigned char ch;
  unsigned int index;

  vector unsigned char src_va_uchar;
  vector unsigned char expected_vresult_uchar;

  vector unsigned int vresult_uint;
  vector unsigned int expected_vresult_uint;
  vector unsigned int src_va_uint;
  vector unsigned int src_vb_uint;
  unsigned int src_a_uint;

  vector int vresult_int;
  vector int expected_vresult_int;
  vector int src_va_int;
  vector int src_vb_int;
  int src_a_int;

  vector unsigned long long int vresult_ullint;
  vector unsigned long long int expected_vresult_ullint;
  vector unsigned long long int src_va_ullint;
  vector unsigned long long int src_vb_ullint;
  unsigned int long long src_a_ullint;

  vector long long int vresult_llint;
  vector long long int expected_vresult_llint;
  vector long long int src_va_llint;
  vector long long int src_vb_llint;
  long long int src_a_llint;

  vector float vresult_float;
  vector float expected_vresult_float;
  vector float src_va_float;
  float src_a_float;

  vector double vresult_double;
  vector double expected_vresult_double;
  vector double src_va_double;
  double src_a_double;

  vector unsigned char vresult_uchar;

  /* Vector replace 32-bit element */
  src_a_uint = 345;
  src_va_uint = (vector unsigned int) { 0, 1, 2, 3 };
  vresult_uint = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_uint = (vector unsigned int) { 0, 1, 345, 3 };
						 
  vresult_uint = vec_replace_elt (src_va_uint, src_a_uint, 2);

  if (!vec_all_eq (vresult_uint, expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_uint, src_va_uint, index)\n");
    for (i = 0; i < 4; i++)
      printf(" vresult_uint[%d] = %d, expected_vresult_uint[%d] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }

  src_a_int = 234;
  src_va_int = (vector int) { 0, 1, 2, 3 };
  vresult_int = (vector int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector int) { 0, 234, 2, 3 };
						 
  vresult_int = vec_replace_elt (src_va_int, src_a_int, 1);

  if (!vec_all_eq (vresult_int, expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_int, src_a_int, index)\n");
    for (i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }
  
  src_a_float = 34.0;
  src_va_float = (vector float) { 0.0, 10.0, 20.0, 30.0 };
  vresult_float = (vector float) { 0.0, 0.0, 0.0, 0.0 };
  expected_vresult_float = (vector float) { 0.0, 34.0, 20.0, 30.0 };
						 
  vresult_float = vec_replace_elt (src_va_float, src_a_float, 1);

  if (!vec_all_eq (vresult_float, expected_vresult_float)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_float, src_a_float, index)\n");
    for (i = 0; i < 4; i++)
      printf(" vresult_float[%d] = %f, expected_vresult_float[%d] = %f\n",
	     i, vresult_float[i], i, expected_vresult_float[i]);
#else
    abort();
#endif
  }

  /* Vector replace 64-bit element */
  src_a_ullint = 456;
  src_va_ullint = (vector unsigned long long int) { 0, 1 };
  vresult_ullint = (vector unsigned long long int) { 0, 0 };
  expected_vresult_ullint = (vector unsigned long long int) { 0, 456 };
						 
  vresult_ullint = vec_replace_elt (src_va_ullint, src_a_ullint, 1);

  if (!vec_all_eq (vresult_ullint,  expected_vresult_ullint)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_ullint, src_a_ullint, index)\n");
    for (i = 0; i < 2; i++)
      printf(" vresult_ullint[%d] = %d, expected_vresult_ullint[%d] = %d\n",
	     i, vresult_ullint[i], i, expected_vresult_ullint[i]);
#else
    abort();
#endif
  }

  src_a_llint = 678;
  src_va_llint = (vector long long int) { 0, 1 };
  vresult_llint = (vector long long int) { 0, 0 };
  expected_vresult_llint = (vector long long int) { 0, 678 };
						 
  vresult_llint = vec_replace_elt (src_va_llint, src_a_llint, 1);

  if (!vec_all_eq (vresult_llint, expected_vresult_llint)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_llint, src_a_llint, index)\n");
    for (i = 0; i < 2; i++)
      printf(" vresult_llint[%d] = %d, expected_vresult_llint[%d] = %d\n",
	     i, vresult_llint[i], i, expected_vresult_llint[i]);
#else
    abort();
#endif
  }
  
  src_a_double = 678.0;
  src_va_double = (vector double) { 0.0, 50.0 };
  vresult_double = (vector double) { 0.0, 0.0 };
  expected_vresult_double = (vector double) { 0.0, 678.0 };
						 
  vresult_double = vec_replace_elt (src_va_double, src_a_double, 1);

  if (!vec_all_eq (vresult_double, expected_vresult_double)) {
#if DEBUG
    printf("ERROR, vec_replace_elt (src_vb_double, src_a_double, index)\n");
    for (i = 0; i < 2; i++)
      printf(" vresult_double[%d] = %f, expected_vresult_double[%d] = %f\n",
	     i, vresult_double[i], i, expected_vresult_double[i]);
#else
    abort();
#endif
  }


  /* Vector replace 32-bit element, unaligned */
  src_a_uint = 345;
  src_va_uchar = (vector unsigned char) { 1, 0, 0, 0, 2, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  /* Byte index 7 will overwrite part of elements 2 and 3 */
  expected_vresult_uchar
    = (vector unsigned char) { 1, 0, 0, 0, 2, 0, 0, 0,
			       0, 0x59, 0x1, 0, 0, 0, 0, 0 };
						 
  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_uint, 3);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_uint, index)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = 0x%x, expected_vresult_uint[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  src_a_int = 234;
  src_va_uchar = (vector unsigned char) { 1, 0, 0, 0, 0, 0, 0, 0,
					  3, 0, 0, 0, 4, 0, 0, 0 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  /* Byte index 7 will over write part of elements 1 and 2 */
  expected_vresult_uchar = (vector unsigned char) { 1, 0, 0, 0, 0, 0xea, 0, 0,
						    0, 0, 0, 0, 4, 0, 0, 0 };

  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_int, 7);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_int, index)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_int[%d] = 0x%x, expected_vresult_int[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  src_a_float = 34.0;
  src_va_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0x41, 0xa0,
					  5, 6, 7, 8, 0x41, 0xf0, 0, 0};
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar
    = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 8, 0x42,
			       5, 6, 7, 8, 0x41, 0xf0, 0, 0 };
						 
  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_float, 8);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_float, "
	   "index)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = 0x%x, expected_vresult_uchar[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  /* Vector replace 64-bit element, unaligned  */
  src_a_ullint = 456;
  src_va_uchar = (vector unsigned char) { 0, 0xc, 0x1, 0, 0, 0, 0, 0,
					  0x22, 0x2, 0, 0, 0, 0, 0, 0 };
  vresult_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					   0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar
    = (vector unsigned char) { 0, 0xc8, 0x1, 0, 0, 0, 0, 0,
			       0, 2, 0, 0, 0, 0, 0, 0 };
						 
  /* Byte index 7 will over write least significant byte of  element 0  */
  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_ullint, 7);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_ullint, "
	   "index)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = 0x%x, expected_vresult_uchar[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }

  src_a_llint = 678;
  src_va_uchar = (vector unsigned char) { 0, 0xa6, 0x2, 0, 0, 0, 0, 0,
					  0x0, 0x1, 0, 0, 0, 0, 0, 0 };
  vresult_llint = (vector long long int) { 0, 0 };
  /* Byte index 7 will over write least significant byte of  element 0  */
  expected_vresult_uchar
    = (vector unsigned char) { 0x0, 0xa6, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0,
			       0x0, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
						 
  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_llint, 7);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_llint, "
	   "index)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = 0x%x, expected_vresult_uchar[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }
  
  src_a_double = 678.0;
  src_va_uchar = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_uchar
    = (vector unsigned char) { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
			       0x0, 0x0, 0x0, 0x0, 0x0, 0x30, 0x85, 0x40 };
						 
  vresult_uchar = vec_replace_unaligned (src_va_uchar, src_a_double, 0);

  if (!vec_all_eq (vresult_uchar, expected_vresult_uchar)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned (src_va_uchar, src_a_double, "
	   "0)\n");
    for (i = 0; i < 16; i++)
      printf(" vresult_uchar[%d] = 0x%x, expected_vresult_uchar[%d] = 0x%x\n",
	     i, vresult_uchar[i], i, expected_vresult_uchar[i]);
#else
    abort();
#endif
  }
  
  return 0;
}

/* { dg-final { scan-assembler-times {\mvinsw\M} 6 } } */
/* { dg-final { scan-assembler-times {\mvinsd\M} 6 } } */
