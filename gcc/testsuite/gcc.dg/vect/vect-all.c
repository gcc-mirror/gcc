/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int iadd_results[N] = {0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90};
float fadd_results[N] = {0.0,6.0,12.0,18.0,24.0,30.0,36.0,42.0,48.0,54.0,60.0,66.0,72.0,78.0,84.0,90.0};
float fmul_results[N] = {0.0,3.0,12.0,27.0,48.0,75.0,108.0,147.0,192.0,243.0,300.0,363.0,432.0,507.0,588.0,675.0};
float fresults1[N] = {192.00,240.00,288.00,336.00,384.00,432.00,480.00,528.00,48.00,54.00,60.00,66.00,72.00,78.00,84.00,90.00};
float fresults2[N] = {0.00,6.00,12.00,18.00,24.00,30.00,36.00,42.00,0.00,54.00,120.00,198.00,288.00,390.00,504.00,630.00};

/****************************************************/
__attribute__ ((noinline))
void icheck_results (int *a, int *results)
{
  int i;
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }
}

__attribute__ ((noinline))
void fcheck_results (float *a, float *results)
{
  int i;
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }
}   

__attribute__ ((noinline)) void 
fbar_mul (float *a)
{
  fcheck_results (a, fmul_results);
} 

__attribute__ ((noinline)) void 
fbar_add (float *a)
{
  fcheck_results (a, fadd_results);
} 

__attribute__ ((noinline)) void 
ibar_add (int *a)
{
  icheck_results (a, iadd_results);
} 

__attribute__ ((noinline)) void 
fbar1 (float *a)
{
  fcheck_results (a, fresults1);
} 

__attribute__ ((noinline)) void 
fbar2 (float *a)
{
  fcheck_results (a, fresults2);
} 

float a[N];
float e[N];
float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
float d[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int ia[N];
char cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
char ca[N];
short sa[N];

/* All of the loops below are currently vectorizable.  */

__attribute__ ((noinline)) int
main1 ()
{
  int i,j;

  /* Test 1: copy chars.  */
  for (i = 0; i < N; i++)
    {
      ca[i] = cb[i];
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ca[i] != cb[i])
	abort ();
    }


  /* Test 2: fp mult.  */
  for (i = 0; i < N; i++)
    {
      a[i] = b[i] * c[i];
    }
  fbar_mul (a);


  /* Test 3: mixed types (int, fp), same nunits in vector.  */
  for (i = 0; i < N; i++)
    {
      a[i] = b[i] + c[i] + d[i];
      e[i] = b[i] + c[i] + d[i];
      ia[i] = ib[i] + ic[i];
    }
  ibar_add (ia);
  fbar_add (a);
  fbar_add (e);


  /* Test 4: access with offset.  */
  for (i = 0; i < N/2; i++)
    {
      a[i] = b[i+N/2] * c[i+N/2] - b[i] * c[i];
      e[i+N/2] = b[i] * c[i+N/2] + b[i+N/2] * c[i];
    }
  fbar1 (a);
  fbar2 (e);


  /* Test 5: access with offset */
  for (i = 1; i <=N-4; i++)
    {
      a[i+3] = b[i-1];
    }
  /* check results:  */
#pragma GCC novector
  for (i = 1; i <=N-4; i++)
    {
      if (a[i+3] != b[i-1])
	abort ();
    }


  /* Test 6 - loop induction with stride != 1.  */
  i = 0;
  j = 0;
  while (i < 5*N)
    {
      a[j] = c[j];
      i += 5;
      j++;
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (a[i] != c[i])
        abort ();
    }


  /* Test 7 - reverse access.  */
  for (i = N; i > 0; i--)
    {
      a[N-i] = d[N-i];
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (a[i] != d[i])
        abort ();
    }


  /* Tests 8,9,10 - constants.  */
  for (i = 0; i < N; i++)
    {
      a[i] = 5.0;
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (a[i] != 5.0)
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      sa[i] = 5;
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (sa[i] != 5)
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + 5;
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + 5)
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 10 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { { vect_aligned_arrays } && {! vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { {! vect_aligned_arrays } && {vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
