/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fchecking" }
   { dg-ice TODO }
   { dg-prune-output {k = 0 \+ \.offset\.[0-9]+;} }
   { dg-prune-output {k = 0 \+ 2;} }
   { dg-prune-output {during IPA pass: \*free_lang_data} } */

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
  {
    int k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k1 } */
    /* { dg-note {variable 'k' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_k1 } */
    for (k = 0; k < 2; k++)
      arr_0 = k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k2 } */
    /* { dg-note {variable 'k' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_k2 } */
    for (k = 0; k < 2; k++)
      arr_0 = k;
  }
}
/* { dg-bogus {error: non-register as LHS of binary operation} {} { xfail *-*-* } .-1 } */
