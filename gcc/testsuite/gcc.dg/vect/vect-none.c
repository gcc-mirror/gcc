/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */

#define N 16

extern void abort (void);

int iadd_results[N] = {0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90};
float fadd_results[N] = {0.0,6.0,12.0,18.0,24.0,30.0,36.0,42.0,48.0,54.0,60.0,66.0,72.0,78.0,84.0,90.0};
float fmul_results[N] = {0.0,3.0,12.0,27.0,48.0,75.0,108.0,147.0,192.0,243.0,300.0,363.0,432.0,507.0,588.0,675.0};
float fresults1[N] = {192.00,240.00,288.00,336.00,384.00,432.00,480.00,528.00,48.00,54.00,60.00,66.00,72.00,78.00,84.00,90.00};
float fresults2[N] = {0.00,6.00,12.00,18.00,24.00,30.00,36.00,42.00,0.00,54.00,120.00,198.00,288.00,390.00,504.00,630.00};

/****************************************************/
void icheck_results (int *a, int *results)
{
  int i;
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }
}

void fcheck_results (float *a, float *results)
{
  int i;
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }
}   

void 
fbar_mul (float *a)
{
  fcheck_results (a, fmul_results);
} 

void 
fbar_add (float *a)
{
  fcheck_results (a, fadd_results);
} 

void 
ibar_add (int *a)
{
  icheck_results (a, iadd_results);
} 

void 
fbar1 (float *a)
{
  fcheck_results (a, fresults1);
} 

void 
fbar2 (float *a)
{
  fcheck_results (a, fresults2);
} 


/* None of the loops below is currently vectorizable. The vectorizer will
   be enhanced to vectorize most of these loops.  */

int
foo (int n)
{
  int i,j;
  float a[N];
  float e[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float d[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
  short sc[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  short sb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  short sa[N];
  int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ia[N];
  int diff = 0;
  char cb[N];
  char cc[N];
  char image[N][N];
  char block[N][N];


  /* Test 1 - type cast.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = (int) sb[i];
    }
  fbar (a);


  /* Test 2 - strided access pattern.  */
  for (i = 0; i < N/2; i++)
    {
      a[i] = b[2*i+1] * c[2*i+1] - b[2*i] * c[2*i];
      d[i] = b[2*i] * c[2*i+1] + b[2*i+1] * c[2*i];
    }
  fbar (a);


  /* Test 3 - no target support for integer mult.  */
  /* This loop is vectorized on platforms that support vect_int_mult.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] * ic[i];
    }
  ibar (ia);


  /* Test 4 - two types with different nunits in vector.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
      sa[i] = sb[i] + sc[i];
    }
  ibar (ia);
  sbar (sa);


  /* Test 5 - too conservative dependence test.  */
  for (i = 0; i < N; i++){
    a[i] = b[i] + c[i];
    a[i+1] = b[i] + c[i];
  }
  fbar (a);


  /* Test 6 - condition in loop.  */
  /* This loop is vectorized on platformst that support vect_condition.  */
  for (i = 0; i < N; i++){
    a[i] = (b[i] > 0 ? b[i] : 0);
  }
  fbar (a);


  /* Test 7 - cross-iteration cycle.  */
  diff = 0;
  for (i = 0; i < N; i++) {
    diff += (cb[i] - cc[i]);
  }
  ibar (&diff);


  /* Test 8 - outer-loop not attempted; inner-loop has cross 
     iteration cycle and multi-dimensional arrays.  */
  diff = 0;
  for (i = 0; i < N; i++) {
    for (i = 0; i < N; i++) {
      diff += (image[i][j] - block[i][j]);
    }
  }
  ibar (&diff);


  /* Test 9 - induction.  */
  for ( i = 0; i < N; i++) {
    a[i] = i;
  }
  fbar (a);


  /* Test 10 - reverse access and forward access.  */
  for (i = N; i > 0; i--)
    {
      a[N-i] = b[i-1];
    }
  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (a[i] != b[N-1-i])
	abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized " 3 "vect"} } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" { xfail powerpc*-*-* i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect" { target powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target i?86-*-* x86_64-*-* ia64-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect" { target i?86-*-* x86_64-*-* ia64-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
