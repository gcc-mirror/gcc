/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

#include <stdio.h>
#include <math.h>

#define MAX_MATRIX_SIZE      (10)

typedef struct
{
  unsigned int nof_rows;
  unsigned int nof_cols;
  float data[MAX_MATRIX_SIZE][MAX_MATRIX_SIZE];
} MATRIX_TYPE;

extern void mtrx_decompose_matrix (MATRIX_TYPE * p_input_matrix);

void
mtrx_decompose_matrix (MATRIX_TYPE * p_input_matrix)
{
  unsigned int row;
  unsigned int col;
  unsigned int sub;
  float sum;
  MATRIX_TYPE tmp;

  for (row = 0; row < MAX_MATRIX_SIZE; row++) {
    for (col = 0; col < MAX_MATRIX_SIZE; col++) {
      tmp.data[row][col] = 0.0;
    }
  }
  tmp.nof_cols = 0;
  tmp.nof_rows = 0;

  for (row = 0; row < p_input_matrix->nof_rows; row++) {
    for (col = row; col < p_input_matrix->nof_cols; col++) {
      sum = 0.0f;
      for (sub = 0; sub < row; sub++) {
	sum += tmp.data[row][sub] * tmp.data[col][sub];
      }
      sum = p_input_matrix->data[col][row] - sum;
      if (row == col) {
	if (sum >= 0.0) {
#if ERROR
	  tmp.data[row][col] = sqrtf (sum);
#else
	  tmp.data[row][col] = sum;
#endif
	}
	else {
	  tmp.data[row][col] = 0.0f;
	}
      }
      else {
	tmp.data[col][row] = sum / tmp.data[row][row];
      }
    }
  }
}

