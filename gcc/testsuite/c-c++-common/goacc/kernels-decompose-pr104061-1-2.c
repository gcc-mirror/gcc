/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-g" } */
/* { dg-additional-options "-O1" } so that we may get some 'GIMPLE_DEBUG's.  */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

int arr_0;

void
foo (void)
{
  /* { dg-bogus {sorry, unimplemented: 'gimple_debug' not yet supported} TODO { xfail *-*-* } .+1 } */
#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'arr_0' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute1 }
     { dg-note {variable 'arr_0' made addressable} {} { target *-*-* } l_compute1 } */
  /* { dg-bogus {note: OpenACC 'kernels' decomposition: variable 'k' declared in block requested to be made addressable} {w/ debug} { xfail *-*-* } l_compute1 } */
  /* { dg-bogus {note: variable 'k' made addressable} {w/ debug} { xfail *-*-* } l_compute1 } */
  /* { dg-note {variable 'arr_0\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { xfail *-*-* } l_compute1 } */
  {
    /* { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {w/ debug} { xfail c++ } .-1 }
       { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {w/ debug} { xfail c } .+1 } */
    int k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k1 } */
    /* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { xfail *-*-* } l_loop_k1 } */
    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { xfail *-*-* } l_loop_k1 } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { xfail *-*-* } l_loop_k1 } */
    for (k = 0; k < 2; k++)
      arr_0 += k;
  }
}
