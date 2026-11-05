/* { dg-do compile } */

#include "arm_neon_test.h"

void
test_left (int8x8_t a, int16x8_t b)
{
  /* Left shifts take 0 to one less than the element width.  */
  vshl_n_s8 (a, 0);
  vshl_n_s8 (a, 7);
  vshl_n_s8 (a, 8); /* { dg-error {passing 8 to argument 2 of 'vshl_n_s8', which expects a value in the range \[0, 7\]} } */
  vqshl_n_s8 (a, 8); /* { dg-error {passing 8 to argument 2 of 'vqshl_n_s8', which expects a value in the range \[0, 7\]} } */
  vqshlu_n_s8 (a, 8); /* { dg-error {passing 8 to argument 2 of 'vqshlu_n_s8', which expects a value in the range \[0, 7\]} } */
  vsli_n_s8 (a, a, 8); /* { dg-error {passing 8 to argument 3 of 'vsli_n_s8', which expects a value in the range \[0, 7\]} } */
}

void
test_right (int8x8_t a, int16x8_t b)
{
  /* Right shifts take 1 to the element width.  */
  vshr_n_s8 (a, 1);
  vshr_n_s8 (a, 8);
  vshr_n_s8 (a, 0); /* { dg-error {passing 0 to argument 2 of 'vshr_n_s8', which expects a value in the range \[1, 8\]} } */
  vshr_n_s8 (a, 9); /* { dg-error {passing 9 to argument 2 of 'vshr_n_s8', which expects a value in the range \[1, 8\]} } */
  vrshr_n_s8 (a, 0); /* { dg-error {passing 0 to argument 2 of 'vrshr_n_s8', which expects a value in the range \[1, 8\]} } */
  vsra_n_s8 (a, a, 0); /* { dg-error {passing 0 to argument 3 of 'vsra_n_s8', which expects a value in the range \[1, 8\]} } */
  vsri_n_s8 (a, a, 0); /* { dg-error {passing 0 to argument 3 of 'vsri_n_s8', which expects a value in the range \[1, 8\]} } */
}

void
test_narrow (int16x8_t a)
{
  /* Narrowing shifts take 1 to the width of the result elements, which is
     half the width of the argument's.  */
  vshrn_n_s16 (a, 8);
  vshrn_n_s16 (a, 9); /* { dg-error {passing 9 to argument 2 of 'vshrn_n_s16', which expects a value in the range \[1, 8\]} } */
  vqshrn_n_s16 (a, 0); /* { dg-error {passing 0 to argument 2 of 'vqshrn_n_s16', which expects a value in the range \[1, 8\]} } */
  vqshrun_n_s16 (a, 9); /* { dg-error {passing 9 to argument 2 of 'vqshrun_n_s16', which expects a value in the range \[1, 8\]} } */
}

void
test_long (int8x8_t a, int8x16_t b)
{
  /* Widening shifts may name the element width itself, which selects SHLL.  */
  vshll_n_s8 (a, 8);
  vshll_n_s8 (a, 9); /* { dg-error {passing 9 to argument 2 of 'vshll_n_s8', which expects a value in the range \[0, 8\]} } */
  vshll_high_n_s8 (b, 9); /* { dg-error {passing 9 to argument 2 of 'vshll_high_n_s8', which expects a value in the range \[0, 8\]} } */
}
