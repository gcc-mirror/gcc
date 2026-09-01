/* Ensure that SIMT functions stack variables work with -msimt.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

struct aggregate {
  int a, b, c, d;
};

struct aggregate global_var = {5, 15, 19, 21};

int __attribute__((noinline,noclone))
f ()
{
  volatile struct aggregate stack_var1 = {1, 2, 3, 4};
  volatile struct aggregate stack_var2 = {5, 6, 7, 8};
  int sum;

  stack_var1.a += __builtin_gcn_dim_pos (2);
  stack_var1.b += __builtin_gcn_dim_pos (2);
  stack_var1.c += __builtin_gcn_dim_pos (2);
  stack_var1.d += __builtin_gcn_dim_pos (2);
  stack_var2.a += __builtin_gcn_dim_pos (2);
  stack_var2.b += __builtin_gcn_dim_pos (2);
  stack_var2.c += __builtin_gcn_dim_pos (2);
  stack_var2.d += __builtin_gcn_dim_pos (2);

  sum = global_var.a + global_var.b + global_var.c + global_var.d;
  sum += stack_var1.a + stack_var1.b + stack_var1.c + stack_var1.d;
  sum += stack_var2.a + stack_var2.b + stack_var2.c + stack_var2.d;

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
