/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target ppc_mma_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <stdio.h>
#include <stdlib.h>
#include <altivec.h>

typedef unsigned char vec_t __attribute__ ((vector_size (16)));
typedef float v4sf_t __attribute__ ((vector_size (16)));
#define SAVE_ACC(ACC, ldc,J)  \
	  __builtin_mma_disassemble_acc (result, ACC); \
	  rowC = (v4sf_t *) &CO[0*ldc+J]; \
          rowC[0] += result[0]; \
          rowC = (v4sf_t *) &CO[1*ldc+J]; \
          rowC[0] += result[1]; \
          rowC = (v4sf_t *) &CO[2*ldc+J]; \
          rowC[0] += result[2]; \
          rowC = (v4sf_t *) &CO[3*ldc+J]; \
	  rowC[0] += result[3];

#define SAVE_ACC1(ACC,ldc, J)  \
	  __builtin_mma_disassemble_acc (result, ACC); \
	  rowC = (v4sf_t *) &CO[4* ldc+J]; \
          rowC[0] += result[0]; \
          rowC = (v4sf_t *) &CO[5*ldc+J]; \
          rowC[0] += result[1]; \
          rowC = (v4sf_t *) &CO[6*ldc+J]; \
          rowC[0] += result[2]; \
          rowC = (v4sf_t *) &CO[7*ldc+J]; \
	  rowC[0] += result[3];
void
MMA (int m, int n, int k, float *A, float *B, float *C)
{
  __vector_quad acc0, acc1, acc2, acc3, acc4, acc5, acc6, acc7;
  v4sf_t result[4];
  v4sf_t *rowC;
  for (int l = 0; l < n; l += 8)
    {
      float *CO;
      float *AO;
      AO = A;
      CO = C;
      C += m * 8;
      for (int j = 0; j < m; j += 16)
	{
	  float *BO = B;
	  __builtin_mma_xxsetaccz (&acc0);
	  __builtin_mma_xxsetaccz (&acc1);
	  __builtin_mma_xxsetaccz (&acc2);
	  __builtin_mma_xxsetaccz (&acc3);
	  __builtin_mma_xxsetaccz (&acc4);
	  __builtin_mma_xxsetaccz (&acc5);
	  __builtin_mma_xxsetaccz (&acc6);
	  __builtin_mma_xxsetaccz (&acc7);
	  unsigned long i;

	  for (i = 0; i < k; i++)
	    {
	      vec_t *rowA = (vec_t *) & AO[i * 16];
	      vec_t *rowB = (vec_t *) & BO[i * 8];
	      __builtin_mma_xvf32gerpp (&acc0, rowB[0], rowA[0]);
	      __builtin_mma_xvf32gerpp (&acc1, rowB[1], rowA[0]);
	      __builtin_mma_xvf32gerpp (&acc2, rowB[0], rowA[1]);
	      __builtin_mma_xvf32gerpp (&acc3, rowB[1], rowA[1]);
	      __builtin_mma_xvf32gerpp (&acc4, rowB[0], rowA[2]);
	      __builtin_mma_xvf32gerpp (&acc5, rowB[1], rowA[2]);
	      __builtin_mma_xvf32gerpp (&acc6, rowB[0], rowA[3]);
	      __builtin_mma_xvf32gerpp (&acc7, rowB[1], rowA[3]);
	    }
	  SAVE_ACC (&acc0, m, 0);
	  SAVE_ACC (&acc2, m, 4);
	  SAVE_ACC1 (&acc1, m, 0);
	  SAVE_ACC1 (&acc3, m, 4);
	  SAVE_ACC (&acc4, m, 8);
	  SAVE_ACC (&acc6, m, 12);
	  SAVE_ACC1 (&acc5, m, 8);
	  SAVE_ACC1 (&acc7, m, 12);
	  AO += k * 16;
	  BO += k * 8;
	  CO += 16;
	}
      B += k * 8;
    }
}

void
init (float *matrix, int row, int column)
{
  for (int j = 0; j < column; j++)
    {
      for (int i = 0; i < row; i++)
	{
	  matrix[j * row + i] = (i * 16 + 2 + j) / 0.123;
	}
    }
}

void
init0 (float *matrix, float *matrix1, int row, int column)
{
  for (int j = 0; j < column; j++)
    for (int i = 0; i < row; i++)
      matrix[j * row + i] = matrix1[j * row + i] = 0;
}


void
print (const char *name, const float *matrix, int row, int column)
{
  printf ("Matrix %s has %d rows and %d columns:\n", name, row, column);
  for (int i = 0; i < row; i++)
    {
      for (int j = 0; j < column; j++)
	{
	  printf ("%f ", matrix[j * row + i]);
	}
      printf ("\n");
    }
  printf ("\n");
}

int
main (int argc, char *argv[])
{
  int rowsA, colsB, common;
  int i, j, k;
  int ret = 0;

  for (int t = 16; t <= 128; t += 16)
    {
      for (int t1 = 8; t1 <= 16; t1 += 8)
	{
	  rowsA = t;
	  colsB = t1;
	  common = 1;
	  /* printf ("Running test for rows = %d,cols = %d\n", t, t1); */
	  float A[rowsA * common];
	  float B[common * colsB];
	  float C[rowsA * colsB];
	  float D[rowsA * colsB];


	  init (A, rowsA, common);
	  init (B, common, colsB);
	  init0 (C, D, rowsA, colsB);
	  MMA (rowsA, colsB, common, A, B, C);

	  for (i = 0; i < colsB; i++)
	    {
	      for (j = 0; j < rowsA; j++)
		{
		  D[i * rowsA + j] = 0;
		  for (k = 0; k < common; k++)
		    {
		      D[i * rowsA + j] +=
			A[k * rowsA + j] * B[k + common * i];
		    }
		}
	    }
	  for (i = 0; i < colsB; i++)
	    {
	      for (j = 0; j < rowsA; j++)
		{
		  for (k = 0; k < common; k++)
		    {
		      if (D[i * rowsA + j] != C[i * rowsA + j])
			{
			  printf ("Error %d,%d,%d\n",i,j,k);
			  ret++;
			}
		    }
		}
	    }
	  if (ret)
	    {
	      print ("A", A, rowsA, common);
	      print ("B", B, common, colsB);
	      print ("C", C, rowsA, colsB);
	      print ("D", D, rowsA, colsB);
	    }
	}
    }

#ifdef VERBOSE
  if (ret)
    printf ("MMA single test fail: %d errors\n",ret);
  else
    printf ("MMA single test success: 0 MMA errors\n");
#else
  if (ret)
    abort();
#endif
      
  return ret;
}
