/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fchecking" }
   { dg-ice TODO }
   { dg-prune-output {D\.[0-9]+ = arr_0\.0 \+ k;} }
   { dg-prune-output {D\.[0-9]+ = arr_0\.1 \+ k;} }
   { dg-prune-output {during GIMPLE pass: lower} } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

int arr_0;

void
foo (void)
{
#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'k' declared in block made addressable} {} { target *-*-* } l_compute1 } */
  /* { dg-note {variable 'k' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute1 } */
  /* { dg-note {variable 'arr_0\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  /* { dg-note {variable 'arr_0\.1' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  {
    int k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k1 } */
    /* { dg-note {variable 'k' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_k1 } */
    for (k = 0; k < 2; k++)
      arr_0 += k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k2 } */
    /* { dg-note {variable 'k' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_k2 } */
    for (k = 0; k < 2; k++)
      arr_0 += k;
      /* { dg-bogus {error: invalid operands in binary operation} {} { xfail *-*-* } .-1 } */
  }
}
