/* Exercise nested function decomposition, gcc/tree-nested.c.  */
/* See gcc/testsuite/gfortran.dg/goacc/nested-function-1.f90 for the Fortran
   version.  */

int main ()
{
#define N 100
  int nonlocal_arg;
  int nonlocal_a[N];
  int nonlocal_i;
  int nonlocal_j;

  for (int i = 0; i < N; ++i)
    nonlocal_a[i] = 5;
  nonlocal_arg = 5;

  void local ()
  {
    int local_i;
    int local_arg;
    int local_a[N];
    int local_j;

    for (int i = 0; i < N; ++i)
      local_a[i] = 5;
    local_arg = 5;

#pragma acc kernels loop \
  gang(num:local_arg) worker(local_arg) vector(local_arg) \
  wait async(local_arg)
    for (local_i = 0; local_i < N; ++local_i)
      {
#pragma acc cache (local_a[local_i:5])
	local_a[local_i] = 100;
#pragma acc loop seq tile(*)
	for (local_j = 0; local_j < N; ++local_j)
	  ;
#pragma acc loop auto independent tile(1)
	for (local_j = 0; local_j < N; ++local_j)
	  ;
      }

#pragma acc kernels loop \
  gang(static:local_arg) worker(local_arg) vector(local_arg) \
  wait(local_arg, local_arg + 1, local_arg + 2) async
    for (local_i = 0; local_i < N; ++local_i)
      {
#pragma acc cache (local_a[local_i:4])
	local_a[local_i] = 100;
#pragma acc loop seq tile(1)
	for (local_j = 0; local_j < N; ++local_j)
	  ;
#pragma acc loop auto independent tile(*)
	for (local_j = 0; local_j < N; ++local_j)
	  ;
      }
  }

  void nonlocal ()
  {
    for (int i = 0; i < N; ++i)
      nonlocal_a[i] = 5;
    nonlocal_arg = 5;

#pragma acc kernels loop \
  gang(num:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) \
  wait async(nonlocal_arg)
    for (nonlocal_i = 0; nonlocal_i < N; ++nonlocal_i)
      {
#pragma acc cache (nonlocal_a[nonlocal_i:3])
	nonlocal_a[nonlocal_i] = 100;
#pragma acc loop seq tile(2)
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
#pragma acc loop auto independent tile(3)
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
      }

#pragma acc kernels loop \
  gang(static:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) \
  wait(nonlocal_arg, nonlocal_arg + 1, nonlocal_arg + 2) async
    for (nonlocal_i = 0; nonlocal_i < N; ++nonlocal_i)
      {
#pragma acc cache (nonlocal_a[nonlocal_i:2])
	nonlocal_a[nonlocal_i] = 100;
#pragma acc loop seq tile(*)
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
#pragma acc loop auto independent tile(*)
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
      }
  }

  local ();
  nonlocal ();

  return 0;
}
