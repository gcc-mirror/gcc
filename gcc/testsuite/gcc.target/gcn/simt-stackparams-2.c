/* Ensure that function calls with stack parameters work with -msimt
   when calling from a SIMT function.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

int __attribute__((noinline,noclone))
f (int a, int b, int c, int d, int e, int f,
   int x, int y, int z)
{
  return a + b + c + d + e + f + x + y + z;
}

int __attribute__((noinline,noclone))
intermediate (int *res, int N)
{
  int i;
  for (i = __builtin_gcn_dim_pos (2); i < N; i += 64)
    res[i] = f (i, 2, 3, 4, 5, 6, 10, 11, 10+i);
}

#define N 256
int res[N];  /* Ensure this isn't a stack-variable.  */

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  /* Check single-threaded works.  */
  for (i = 0; i < N; i++)
    if (f (i, 2, 3, 4, 5, 6, 10, 11, 10+i) != 51 + 2*i)
      __builtin_exit (1);

  /* Check multithreaded works.  */
  A = __builtin_gcn_simt_enter (1, 2);
  intermediate (res, N);
  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (i = 0; i < N; i++)
    if (res[i] != 51 + 2*i)
      __builtin_exit (2);
  return 0;
}
