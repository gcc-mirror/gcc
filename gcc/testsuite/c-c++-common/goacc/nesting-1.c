/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

extern int i;

void
f_acc_parallel (void)
{
#pragma acc parallel
  {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
       { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < 2; ++i)
      ;
  }
}


void
f_acc_kernels (void)
{
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < 2; ++i)
      ;
  }
}


void
f_acc_serial (void)
{
#pragma acc serial
  {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
       { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < 2; ++i)
      ;
  }
}


void
f_acc_data (void)
{
#pragma acc data
  {
#pragma acc parallel
    ;

#pragma acc parallel
    {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
      /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
	 { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (i = 0; i < 2; ++i)
	;
    }

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } l_compute$c_compute } */
    ;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (i = 0; i < 2; ++i)
	;
    }

#pragma acc serial
    ;

#pragma acc serial
    {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
      /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
	 { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (i = 0; i < 2; ++i)
	;
    }

#pragma acc data
    ;

#pragma acc update host(i)

#pragma acc enter data copyin(i)

#pragma acc exit data delete(i)

#pragma acc data
    {
#pragma acc parallel
      ;

#pragma acc parallel
      {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
	/* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
	   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
	for (i = 0; i < 2; ++i)
	  ;
      }

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
      /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } l_compute$c_compute } */
      ;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
      /* { dg-note {variable 'i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
      {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
	/* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
	for (i = 0; i < 2; ++i)
	  ;
      }

#pragma acc serial
      ;

#pragma acc serial
      {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
	/* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i }
	   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
	for (i = 0; i < 2; ++i)
	  ;
      }

#pragma acc data
      ;

#pragma acc update host(i)

#pragma acc enter data copyin(i)

#pragma acc exit data delete(i)
    }
  }
}
