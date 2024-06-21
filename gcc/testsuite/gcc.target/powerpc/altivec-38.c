/* { dg-do run { target vmx_hw } } */
/* { dg-do compile { target { ! vmx_hw } } } */
/* { dg-options "-O2 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

/* Endian considerations: The "high" half of a vector with n elements is the
   first n/2 elements of the vector. For little endian, these elements are in
   the rightmost half of the vector. For big endian, these elements are in the
   leftmost half of the vector.  */


void abort (void);

int main ()
{
  int i;
  vector bool short vec_bs_arg;
  vector bool short vec_bs_result, vec_bs_expected;
  vector bool int vec_bi_arg;
  vector bool int vec_bi_result, vec_bi_expected;
  vector bool char vec_bc_arg;
  vector bool char vec_bc_result, vec_bc_expected;
  vector signed short vec_ss_arg;
  vector signed short vec_ss_result, vec_ss_expected;
  vector signed int vec_si_arg;
  vector signed int vec_si_result, vec_si_expected;
  vector signed char vec_sc_arg;
  vector signed char vec_sc_result, vec_sc_expected;
  vector pixel vec_pixel_arg;
  vector unsigned int vec_ui_result, vec_ui_expected;

  vec_bs_arg = (vector bool short){ 0, 101, 202, 303,
				    404, 505, 606, 707 };
  vec_bi_expected = (vector bool int){ 0, 101, 202, 303 };

  vec_bi_result = vec_unpackh (vec_bs_arg);

  for (i = 0; i < 4; i++) {
    if (vec_bi_expected[i] != vec_bi_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(),  vec_bi_expected[%d] = %d does not match vec_bi_result[%d] = %d\n",
	      i, vec_bi_expected[i], i, vec_bi_result[i]);
#else
       abort();
#endif
  }

  vec_bi_expected = (vector bool int){ 404, 505, 606, 707 };
  vec_bi_result = vec_unpackl (vec_bs_arg);

  for (i = 0; i < 4; i++) {
    if (vec_bi_expected[i] != vec_bi_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl(), vec_bi_expected[%d] = %d does not match vec_bi_result[%d] = %d\n",
	      i, vec_bi_expected[i], i, vec_bi_result[i]);
#else
       abort();
#endif
  }

  
  vec_ss_arg = (vector signed short){ 0, 101, 202, 303,
				    404, 505, 606, 707 };
  vec_si_expected = (vector signed int){ 0, 101, 202, 303 };

  vec_si_result = vec_unpackh (vec_ss_arg);

  for (i = 0; i < 4; i++) {
    if (vec_si_expected[i] != vec_si_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(), vec_si_expected[%d] = %d does not match vec_si_result[%d] = %d\n",
	      i, vec_si_expected[i], i, vec_si_result[i]);
#else
       abort();
#endif
  }

  vec_si_expected = (vector signed int){ 404, 505, 606, 707 };

  vec_si_result = vec_unpackl (vec_ss_arg);

  for (i = 0; i < 4; i++) {
    if (vec_si_expected[i] != vec_si_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl(), vec_si_expected[%d] = %d does not match vec_si_result[%d] = %d\n",
	      i, vec_si_expected[i], i, vec_si_result[i]);
#else
       abort();
#endif
  }


  vec_pixel_arg = (vector pixel){ 0x0, 0x65, 0xca, 0x12f,
				  0x194, 0x1f9, 0x25e, 0x2c3 };
  vec_ui_expected = (vector unsigned int){ 0x0, 0x305, 0x60a, 0x90f };

  vec_ui_result = vec_unpackh (vec_pixel_arg);

  for (i = 0; i < 4; i++) {
    if (vec_ui_expected[i] != vec_ui_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(), vec_ui_expected[%d] = 0x%x does not match vec_ui_result[%d] = 0x%x\n",
	      i, vec_ui_expected[i], i, vec_ui_result[i]);
#else
       abort();
#endif
  }

  vec_ui_expected = (vector unsigned int){ 0xc14, 0xf19, 0x121e, 0x1603 };

  vec_ui_result = vec_unpackl (vec_pixel_arg);

  for (i = 0; i < 4; i++) {
    if (vec_ui_expected[i] != vec_ui_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl(), vec_ui_expected[%d] = 0x%x does not match vec_ui_result[%d] = 0x%x\n",
	      i, vec_ui_expected[i], i, vec_ui_result[i]);
#else
       abort();
#endif
  }


  vec_bc_arg = (vector bool char){ 0, 1, 0, 1, 0, 1, 0, 1,
				   0, 0, 1, 1, 0, 0, 1, 1 };

  vec_bs_expected = (vector bool short){ 0, 1, 0, 1, 0, 1, 0, 1 };

  vec_bs_result = vec_unpackh (vec_bc_arg);

  for (i = 0; i < 8; i++) {
    if (vec_bs_expected[i] != vec_bs_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(), vec_bs_expected[%d] = %d does not match vec_bs_result[%d] = %d\n",
	      i, vec_bs_expected[i], i, vec_bs_result[i]);
#else
       abort();
#endif
  }

  vec_bs_expected = (vector bool short){ 0, 0, 1, 1, 0, 0, 1, 1 };

  vec_bs_result = vec_unpackl (vec_bc_arg);

  for (i = 0; i < 8; i++) {
    if (vec_bs_expected[i] != vec_bs_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(), vec_bs_expected[%d] = %d does not match vec_bs_result[%d] = %d\n",
	      i, vec_bs_expected[i], i, vec_bs_result[i]);
#else
       abort();
#endif
  }

  vec_bs_expected = (vector bool short){ 0, 0, 1, 1, 0, 0, 1, 1 };

  vec_bs_result = vec_unpackl (vec_bc_arg);

  for (i = 0; i < 8; i++) {
    if (vec_bs_expected[i] != vec_bs_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl(), vec_bs_expected[%d] = %d does not match vec_bs_result[%d] = %d\n",
	      i, vec_bs_expected[i], i, vec_bs_result[i]);
#else
       abort();
#endif
  }


  vec_sc_arg = (vector signed char){ 0, 1, 2, 3, 4, 5, 6, 7,
				     8, 9, 10, 11, 12, 13, 14, 15 };

  vec_ss_expected = (vector signed short){ 0, 1, 2, 3, 4, 5, 6, 7 };

  vec_ss_result = vec_unpackh (vec_sc_arg);

  for (i = 0; i < 8; i++) {
    if (vec_ss_expected[i] != vec_ss_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh(), vec_ss_expected[%d] = %d does not match vec_ss_result[%d] = %d\n",
	      i, vec_ss_expected[i], i, vec_ss_result[i]);
#else
       abort();
#endif
  }

  vec_ss_expected = (vector signed short){ 8, 9, 10, 11, 12, 13, 14, 15 };

  vec_ss_result = vec_unpackl (vec_sc_arg);

  for (i = 0; i < 8; i++) {
    if (vec_ss_expected[i] != vec_ss_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl(), vec_ss_expected[%d] = %d does not match vec_ss_result[%d] = %d\n",
	      i, vec_ss_expected[i], i, vec_ss_result[i]);
#else
       abort();
#endif
  }

  return 0;
}
