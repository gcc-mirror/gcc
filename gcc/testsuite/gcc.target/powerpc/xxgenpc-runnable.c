/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */
/* { dg-require-effective-target power10_ok } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#define IMM0 0
#define IMM1 1
#define IMM2 2
#define IMM3 3

void abort (void);

int main ()
{
  int i;
  vector unsigned char vec_byte_arg;
  vector unsigned char vec_byte_result, vec_byte_expected0, vec_byte_expected1;
  vector unsigned char vec_byte_expected2, vec_byte_expected3;

  vector unsigned short vec_hword_arg;
  vector unsigned short vec_hword_result, vec_hword_expected0;
  vector unsigned short vec_hword_expected1, vec_hword_expected2;
  vector unsigned short vec_hword_expected3;

  vector unsigned int vec_word_arg;
  vector unsigned int vec_word_result, vec_word_expected0, vec_word_expected1;
  vector unsigned int vec_word_expected2, vec_word_expected3;

  vec_byte_arg = (vector unsigned char ){ 0xFF, 0xF0, 0x7F, 0xFF,
					  0xFF, 0xFF, 0xFF, 0xFF,
					  0x00, 0x00, 0x01, 0x23,
					  0x45, 0x67, 0x00, 0x00 };

  vec_byte_result = (vector unsigned char ){ 0xFF, 0xFF, 0xFF, 0xFF,
					     0xFF, 0xFF, 0xFF, 0xFF,
					     0xFF, 0xFF, 0xFF, 0xFF,
					     0xFF, 0xFF, 0xFF, 0xFF };

  vec_byte_expected0 = (vector unsigned char){ 0x1F, 0x1E, 0x1D, 0x1C,
					       0x1B, 0x1A, 0x19, 0x18,
					       0x06, 0x05, 0x15, 0x04,
					       0x03, 0x02, 0x01, 0x00 };

  vec_byte_expected1 = (vector unsigned char){ 0x00, 0x00, 0x00, 0x00,
					       0x00, 0x00, 0x00, 0x00,
					       0x00, 0x07, 0x06, 0x04,
					       0x03, 0x02, 0x01, 0x00 };

  vec_byte_expected2 = (vector unsigned char){ 0x10, 0x11, 0x12, 0x13,
					       0x14, 0x15, 0x16, 0x17,
					       0x00, 0x01, 0x1a, 0x02,
					       0x03, 0x04, 0x05, 0x06 };

  vec_byte_expected3 = (vector unsigned char){ 0x08, 0x09, 0x0B, 0x0C,
					       0x0D, 0x0E, 0x0F, 0x00,
					       0x00, 0x00, 0x00, 0x00,
					       0x00, 0x00, 0x00, 0x00 };

  vec_hword_arg = (vector unsigned short) { 0x0004, 0xF003, 0x0002, 0x0001,
					    0xF004, 0x1003, 0xF002, 0x0001 };
  vec_hword_expected0 = (vector unsigned short int){ 0x405, 0x1c1d, 0x203,
						     0x1819, 0x1617, 0x1,
						     0x1213, 0x1011 };
  vec_hword_expected1 = (vector unsigned short int){ 0x0, 0x0, 0x0, 0x0,
						     0x0, 0xe0f, 0xa0b, 0x405 };
  vec_hword_expected2 = (vector unsigned short int){ 0x100, 0x1312, 0x302,
						     0x1716, 0x1918, 0x504,
						     0x1d1c, 0x1f1e };
  vec_hword_expected3 = (vector unsigned short int){ 0x100, 0x504, 0xb0a, 0x0,
						     0x0, 0x0, 0x0, 0x0 };

  vec_word_arg = (vector unsigned int){ 0xFEDCBA90, 0xF101, 0xF0000202, 0xF303 };
  vec_word_expected0 = (vector unsigned int){ 0x4050607, 0x18191a1b,
					      0x10203, 0x10111213 };
  vec_word_expected1 = (vector unsigned int){ 0x0, 0x0, 0xc0d0e0f, 0x4050607 };
  vec_word_expected2 = (vector unsigned int){ 0x3020100, 0x17161514,
					      0x7060504, 0x1f1e1d1c };
  vec_word_expected3 = (vector unsigned int){ 0x3020100, 0xb0a0908, 0x0, 0x0 };

  vec_byte_result = vec_genpcvm (vec_byte_arg, IMM0);

  for (i = 0; i < 16; i++) {
    if (vec_byte_expected0[i] != vec_byte_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 0), vec_byte_expected[%d] = 0x%x does not match vec_byte_result[%d] = 0x%x\n",
	     i, vec_byte_expected0[i], i, vec_byte_result[i]);
#else
    abort();
#endif
  }

  vec_byte_result = vec_genpcvm (vec_byte_arg, IMM1);

  for (i = 0; i < 16; i++) {
    if (vec_byte_expected1[i] != vec_byte_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 1), vec_byte_expected[%d] = 0x%x does not match vec_byte_result[%d] = 0x%x\n",
	     i, vec_byte_expected1[i], i, vec_byte_result[i]);
#else
    abort();
#endif
  }

  vec_byte_result = vec_genpcvm (vec_byte_arg, IMM2);

  for (i = 0; i < 16; i++) {
    if (vec_byte_expected2[i] != vec_byte_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvmbm(IMM = 2), vec_byte_expected[%d] = 0x%x does not match vec_byte_result[%d] = 0x%x\n",
	     i, vec_byte_expected2[i], i, vec_byte_result[i]);
#else
    abort();
#endif
  }

  vec_byte_result = vec_genpcvm (vec_byte_arg, IMM3);

  for (i = 0; i < 16; i++) {
    if (vec_byte_expected3[i] != vec_byte_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 3), vec_byte_expected[%d] = 0x%x does not match vec_byte_result[%d] = 0x%x\n",
	     i, vec_byte_expected3[i], i, vec_byte_result[i]);
#else
    abort();
#endif
  }

  vec_hword_result = vec_genpcvm (vec_hword_arg, IMM0);

  for (i = 0; i < 8; i++) {
    if (vec_hword_expected0[i] != vec_hword_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvmhm(IMM = 0), vec_hword_expected[%d] = 0x%x does not match vec_hword_result[%d] = 0x%x\n",
	     i, vec_hword_expected0[i], i, vec_hword_result[i]);
#else
    abort();
#endif
  }

  vec_hword_result = vec_genpcvm (vec_hword_arg, IMM1);

  for (i = 0; i < 8; i++) {
    if (vec_hword_expected1[i] != vec_hword_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 1), vec_hword_expected[%d] = 0x%x does not match vec_hword_result[%d] = 0x%x\n",
	     i, vec_hword_expected1[i], i, vec_hword_result[i]);
#else
     abort();
#endif
  }

  vec_hword_result = vec_genpcvm (vec_hword_arg, IMM2);

  for (i = 0; i < 8; i++) {
    if (vec_hword_expected2[i] != vec_hword_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 2), vec_hword_expected[%d] = 0x%x does not match vec_hword_result[%d] = 0x%x\n",
	     i, vec_hword_expected2[i], i, vec_hword_result[i]);
#else
    abort();
#endif
  }

  vec_hword_result = vec_genpcvm (vec_hword_arg, IMM3);

  for (i = 0; i < 8; i++) {
    if (vec_hword_expected3[i] != vec_hword_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 3), vec_hword_expected[%d] = 0x%x does not match vec_hword_result[%d] = 0x%x\n",
	     i, vec_hword_expected3[i], i, vec_hword_result[i]);
#else
    abort();
#endif
  }


  vec_word_result = vec_genpcvm (vec_word_arg, IMM0);

  for (i = 0; i < 4; i++) {
    if (vec_word_expected0[i] != vec_word_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 0), vec_word_expected[%d] = 0x%x does not match vec_word_result[%d] = 0x%x\n",
	     i, vec_word_expected0[i], i, vec_word_result[i]);
#else
    abort();
#endif
  }

  vec_word_result = vec_genpcvm (vec_word_arg, IMM1);

  for (i = 0; i < 4; i++) {
    if (vec_word_expected1[i] != vec_word_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 1), vec_word_expected[%d] = 0%x does not match vec_word_result[%d] = 0x%x\n",
	     i, vec_word_expected1[i], i, vec_word_result[i]);
#else
    abort();
#endif
  }

  vec_word_result = vec_genpcvm (vec_word_arg, IMM2);

  for (i = 0; i < 4; i++) {
    if (vec_word_expected2[i] != vec_word_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 2), vec_word_expected[%d] = 0x%x does not match vec_word_result[%d] = 0x%x\n",
	     i, vec_word_expected2[i], i, vec_word_result[i]);
#else
    abort();
#endif
  }

  vec_word_result = vec_genpcvm (vec_word_arg, IMM3);

  for (i = 0; i < 4; i++) {
    if (vec_word_expected3[i] != vec_word_result[i])
#if DEBUG
      printf("ERROR: vec_genpcvm(IMM = 3), vec_word_expected[%d] = 0x%x does not match vec_word_result[%d] = 0x%x\n",
	     i, vec_word_expected3[i], i, vec_word_result[i]);
#else
    abort();
#endif
  }

  return 0;
}
