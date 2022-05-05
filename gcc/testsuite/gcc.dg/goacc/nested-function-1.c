/* Exercise nested function decomposition, gcc/tree-nested.c.  */
/* See gcc/testsuite/gfortran.dg/goacc/nested-function-1.f90 for the Fortran
   version.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute_loop 0 c_loop 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

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

#pragma acc kernels loop /* { dg-line l_compute_loop[incr c_compute_loop] } */ \
  gang(num:local_arg) worker(local_arg) vector(local_arg) \
  wait async(local_arg)
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'local_arg' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
       { dg-note {variable 'local_arg' made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_arg\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    for (local_i = 0; local_i < N; ++local_i)
      {
#pragma acc cache (local_a[local_i:5])
	local_a[local_i] = 100;
#pragma acc loop seq tile(*) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (local_j = 0; local_j < N; ++local_j)
	  ;
#pragma acc loop auto independent tile(1) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (local_j = 0; local_j < N; ++local_j)
	  ;
      }

#pragma acc kernels loop /* { dg-line l_compute_loop[incr c_compute_loop] } */ \
  gang(static:local_arg) worker(local_arg) vector(local_arg) \
  wait(local_arg, local_arg + 1, local_arg + 2) async
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'local_arg' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
       { dg-note {variable 'local_arg' already made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_arg\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'local_i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    for (local_i = 0; local_i < N; ++local_i)
      {
#pragma acc cache (local_a[local_i:4])
	local_a[local_i] = 100;
#pragma acc loop seq tile(1) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (local_j = 0; local_j < N; ++local_j)
	  ;
#pragma acc loop auto independent tile(*) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (local_j = 0; local_j < N; ++local_j)
	  ;
      }
  }

  void nonlocal ()
  {
    for (int i = 0; i < N; ++i)
      nonlocal_a[i] = 5;
    nonlocal_arg = 5;

#pragma acc kernels loop /* { dg-line l_compute_loop[incr c_compute_loop] } */ \
  gang(num:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) \
  wait async(nonlocal_arg)
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'nonlocal_arg' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
       { dg-note {variable 'nonlocal_arg' made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_arg\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    for (nonlocal_i = 0; nonlocal_i < N; ++nonlocal_i)
      {
#pragma acc cache (nonlocal_a[nonlocal_i:3])
	nonlocal_a[nonlocal_i] = 100;
#pragma acc loop seq tile(2) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
#pragma acc loop auto independent tile(3) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
      }

#pragma acc kernels loop /* { dg-line l_compute_loop[incr c_compute_loop] } */ \
  gang(static:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) \
  wait(nonlocal_arg, nonlocal_arg + 1, nonlocal_arg + 2) async
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'nonlocal_arg' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
       { dg-note {variable 'nonlocal_arg' already made addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_arg\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-note {variable 'nonlocal_i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop } */
    for (nonlocal_i = 0; nonlocal_i < N; ++nonlocal_i)
      {
#pragma acc cache (nonlocal_a[nonlocal_i:2])
	nonlocal_a[nonlocal_i] = 100;
#pragma acc loop seq tile(*) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
#pragma acc loop auto independent tile(*) /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
	for (nonlocal_j = 0; nonlocal_j < N; ++nonlocal_j)
	  ;
      }
  }

  local ();
  nonlocal ();

  return 0;
}
