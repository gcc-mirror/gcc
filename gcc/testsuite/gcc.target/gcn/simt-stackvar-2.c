/* Ensure that SIMT functions stack variables work with -msimt.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

int global_var = 60;

int __attribute__((noinline,noclone))
f ()
{
  volatile int stack_var1[4] = {1, 2, 3, 4};
  volatile int stack_var2[4] = {5, 6, 7, 8};
  int i, sum;

  for (i = 0; i < 4; i++)
    {
      stack_var1[i] += __builtin_gcn_dim_pos (2);
      stack_var2[i] += __builtin_gcn_dim_pos (2);
    }

  sum = global_var;
  for (i = 0; i < 4; i++)
    sum += stack_var1[i] + stack_var2[i];

  return sum;
}

int res[64]; /* Let's not make this a stack variable.  */

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  /* Check multithreaded works.  */
  A = __builtin_gcn_simt_enter (1, 2);
  res[__builtin_gcn_dim_pos (2)] = f ();
  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (i = 0; i < 64; i++)
    if (res[i] != 96 + i*8)
      __builtin_exit (1);
  return 0;
}
