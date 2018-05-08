/* { dg-do run { target { powerpc*-*-* &&  p9vector_hw } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2" } */

#include <stdint.h>
#include <stdio.h>
#include <altivec.h> // vector

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);


int main() {

  vector signed char char_src1, char_src2;
  vector unsigned char uchar_src1, uchar_src2;
  vector signed short int short_src1, short_src2;
  vector unsigned short int ushort_src1, ushort_src2;
  vector signed int int_src1, int_src2;
  vector unsigned int uint_src1, uint_src2;
  unsigned int result, expected_result;


  /* Tests for: vec_first_match_index() */
  /* char */
  char_src1 = (vector signed char) {-1, 2, 3, 4, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, 2, 3, 20, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 0;

  result = vec_first_match_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, -2, -3, -4, -5, -6, -7, -8,
				    -9, -10, -11, -12, -13, -14, -15, -16};
  expected_result = 16;

  result = vec_first_match_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {0, 2, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {1, 0, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 2;

  result = vec_first_match_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16, 17};
  uchar_src2 = (vector unsigned char) {3, 4, 5, 6, 7, 8, 9, 10,
				       11, 12, 13, 14, 15, 16, 17, 18};
  expected_result = 16;

  result = vec_first_match_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* short int */
  short_src1 = (vector short int) {10, -20, -30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {-10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 3;

  result = vec_first_match_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {0, 0, 0, 0, 0, 0, 0, 0};

  expected_result = 8;

  result = vec_first_match_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {0, 0, 0, 0, 0, 60, 70, 0};
  ushort_src2 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 5;

  result = vec_first_match_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {-20, 30, -40, 50,
					     60, -70, 80, -90};
  ushort_src2 = (vector short unsigned int) {20, -30, 40, -50,
					     -60, 70, -80, 90};

  expected_result = 8;

  result = vec_first_match_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* int */
  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {10, 20, 30, 4};

  expected_result = 3;

  result = vec_first_match_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {4, 3, 2, 1};

  expected_result = 4;

  result = vec_first_match_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {11, 2, 33, 4};

  expected_result = 1;

  result = vec_first_match_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {2, 3, 4, 5};

  expected_result = 4;

  result = vec_first_match_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* Tests for: vec_first_mismatch_index() */
  /* char */
  char_src1 = (vector signed char) {-1, 2, 3, 4, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, 2, 3, 20, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 3;

  result = vec_first_mismatch_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				     9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 16;

  result = vec_first_mismatch_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {1, 2, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {1, 0, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 1;

  result = vec_first_mismatch_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       0, 11, 12, 13, 14, 15, 16};
  expected_result = 8;

  result = vec_first_mismatch_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch result (%d) does not match expected result (%d)\n",
	     result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16};
  expected_result = 16;

  result = vec_first_mismatch_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* short int */
  short_src1 = (vector short int) {-10, -20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {-10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 1;

  result = vec_first_mismatch_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 8;

  result = vec_first_mismatch_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 0};
  ushort_src2 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 7;

  result = vec_first_mismatch_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {20, 30, 40, 50, 60, 70, 80, 90};
  ushort_src2 = (vector short unsigned int) {20, 30, 40, 50, 60, 70, 80, 90};

  expected_result = 8;

  result = vec_first_mismatch_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* int */
  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {1, 20, 3, 4};

  expected_result = 1;

  result = vec_first_mismatch_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {1, 2, 3, 4};

  expected_result = 4;

  result = vec_first_mismatch_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 0, 3, 4};
  int_src2 = (vector int) {1, 2, 3, 4};

  expected_result = 1;

  result = vec_first_mismatch_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {11, 2, 33, 4};

  expected_result = 0;

  result = vec_first_mismatch_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {1, 2, 3, 4};

  expected_result = 4;

  result = vec_first_mismatch_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* Tests for: vec_first_match_or_eos_index() */
  /* char */
  char_src1 = (vector signed char) {-1, 2, 3, 4, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, 2, 3, 20, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 0;

  result = vec_first_match_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first match result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {-1, 2, 3, 0, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {2, 3, 20, 0, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 3;

  result = vec_first_match_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, -2, -3, -4, -5, -6, -7, -8,
				    -9, -10, -11, -12, -13, -14, -15, -16};
  expected_result = 16;

  result = vec_first_match_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
     abort();
#endif

  uchar_src1 = (vector unsigned char) {1, 2, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {-1, 0, -3, -4, -5, -6, -7, -8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 1;

  result = vec_first_match_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16, 17};
  uchar_src2 = (vector unsigned char) {3, 4, 5, 6, 7, 8, 9, 10,
				       11, 12, 13, 14, 15, 16, 17, 18};
  expected_result = 16;

  result = vec_first_match_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first match or EOS  result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* short int */
  short_src1 = (vector short int) {10, -20, -30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {-10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 3;

  result = vec_first_match_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {1, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {10, 0, 30, 40, 50, 60, 70, 80};

  expected_result = 1;

  result = vec_first_match_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {-10, -20, -30, -40, -50, -60, -70, -80};
  short_src2 = (vector short int) {10, 20, 30, 40, 50, 0, 70, 80};

  expected_result = 5;

  result = vec_first_match_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {0, 0, 0, 0, 0, 0, 0, 0};

  expected_result = 0;

  result = vec_first_match_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {1, 2, 0, 0, 60, 70, 0};
  ushort_src2 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 2;

  result = vec_first_match_or_eos_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {-20, 30, -40, 50,
					     60, -70, 80, -90};
  ushort_src2 = (vector short unsigned int) {20, -30, 40, -50,
					     -60, 70, -80, 90};

  expected_result = 8;

  result = vec_first_match_or_eos_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif
  ushort_src1 = (vector short unsigned int) {-20, 30, -40, 50,
					     60, -70, 80, 0};
  ushort_src2 = (vector short unsigned int) {20, -30, 40, -50,
					     -60, 70, -80, 90};

  expected_result = 7;

  result = vec_first_match_or_eos_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
     printf("Error: ushort int first match or EOS result (%d) does not match expected result (%d)\n",
	    result, expected_result);
#else
    abort();
#endif

  /* int */
  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {10, 20, 30, 4};

  expected_result = 3;

  result = vec_first_match_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {0, 2, 3, 4};
  int_src2 = (vector int) {4, 3, 2, 1};

  expected_result = 0;

  result = vec_first_match_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {4, 3, 2, 1};

  expected_result = 4;

  result = vec_first_match_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
     printf("Error: int first match or EOS result (%d) does not match expected result (%d)\n",
	    result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {11, 2, 33, 4};

  expected_result = 1;

  result = vec_first_match_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 0, 4};
  uint_src2 = (vector unsigned int) {2, 3, 4, 5};

  expected_result = 2;

  result = vec_first_match_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {2, 3, 4, 5};

  expected_result = 4;

  result = vec_first_match_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first match or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* Tests for: vec_first_mismatch_or_eos_index() */
  /* char */
  char_src1 = (vector signed char) {-1, 2, 3, 4, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {-1, 2, 3, 20, -5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 3;

  result = vec_first_mismatch_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {1, 2, 0, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {1, 2, 0, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 2;

  result = vec_first_mismatch_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  char_src1 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  char_src2 = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				    9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 16;

  result = vec_first_mismatch_or_eos_index (char_src1, char_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: char first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {1, 2, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {1, 0, 3, 4, 5, 6, 7, 8,
				       9, 10, 11, 12, 13, 14, 15, 16};
  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
     abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       0, 11, 12, 13, 14, 15, 16};
  uchar_src2 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       0, 11, 12, 13, 14, 15, 16};
  expected_result = 8;

  result = vec_first_mismatch_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16, 17};
  uchar_src2 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 0, 16, 17};
  expected_result = 13;

  result = vec_first_mismatch_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uchar_src1 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16, 17};
  uchar_src2 = (vector unsigned char) {2, 3, 4, 5, 6, 7, 8, 9,
				       10, 11, 12, 13, 14, 15, 16, 17};
  expected_result = 16;

  result = vec_first_mismatch_or_eos_index (uchar_src1, uchar_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uchar first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* short int */
  short_src1 = (vector short int) {-10, -20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {-10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {0, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {0, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 0;

  result = vec_first_mismatch_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 8;

  result = vec_first_mismatch_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  short_src1 = (vector short int) {10, 0, 30, 40, 50, 60, 70, 80};
  short_src2 = (vector short int) {10, 0, 30, 40, 50, 60, 70, 80};

  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (short_src1, short_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: short int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 0};
  ushort_src2 = (vector short unsigned int) {10, 20, 30, 40, 50, 60, 70, 80};

  expected_result = 7;

  result = vec_first_mismatch_or_eos_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {20, 0, 40, 50, 60, 70, 80, 90};
  ushort_src2 = (vector short unsigned int) {20, 0, 40, 50, 60, 70, 80, 90};

  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (ushort_src1, ushort_src2);

 if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  ushort_src1 = (vector short unsigned int) {20, 30, 40, 50, 60, 70, 80, 90};
  ushort_src2 = (vector short unsigned int) {20, 30, 40, 50, 60, 70, 80, 90};

  expected_result = 8;

  result = vec_first_mismatch_or_eos_index (ushort_src1, ushort_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: ushort int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  /* int */
  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {1, 20, 3, 4};

  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch or EOS result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 2, 3, 4};
  int_src2 = (vector int) {1, 2, 3, 4};

  expected_result = 4;

  result = vec_first_mismatch_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 2, 0, 4};
  int_src2 = (vector int) {1, 2, 0, 4};

  expected_result = 2;

  result = vec_first_mismatch_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  int_src1 = (vector int) {1, 0, 3, 4};
  int_src2 = (vector int) {1, 2, 3, 4};

  expected_result = 1;

  result = vec_first_mismatch_or_eos_index (int_src1, int_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: int first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {11, 2, 33, 4};

  expected_result = 0;

  result = vec_first_mismatch_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 0};
  uint_src2 = (vector unsigned int) {1, 2, 3, 0};

  expected_result = 3;

  result = vec_first_mismatch_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif

  uint_src1 = (vector unsigned int) {1, 2, 3, 4};
  uint_src2 = (vector unsigned int) {1, 2, 3, 4};

  expected_result = 4;

  result = vec_first_mismatch_or_eos_index (uint_src1, uint_src2);

  if (result != expected_result)
#ifdef DEBUG
    printf("Error: uint first mismatch result (%d) does not match expected result (%d)\n",
	   result, expected_result);
#else
    abort();
#endif
}
