/* Reduced from 'libgomp.oacc-c-c++-common/kernels-loop-2.c'.  */

/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

void
foo (void) /* { dg-line l_f_1 } */
{
#pragma acc kernels /* { dg-line l_k_1 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' declared in block requested to be made addressable} {} { target *-*-* } l_k_1 } */
  /* { dg-note {variable 'i' made addressable} {} { target *-*-* } l_k_1 } */
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_k_1 } */
  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_k_1 } */
  /* { dg-bogus {note: beginning 'parloops' part in OpenACC 'kernels' region} {TODO location} { xfail *-*-* } l_f_1 }
     { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} TODO { xfail *-*-* } .+1 } */
  for (int i;;)
    ;
}
