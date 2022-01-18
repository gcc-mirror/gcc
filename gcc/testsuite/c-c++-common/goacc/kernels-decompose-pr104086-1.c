/* Reduced from 'libgomp.oacc-c-c++-common/declare-vla.c'.  */

/* { dg-additional-options "-fchecking" }
   { dg-ice TODO }
   { dg-prune-output {during GIMPLE pass: omplower} } */

/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

void
foo (void)
{
#pragma acc data /* { dg-line l_data1 } */
  /* { dg-bogus {note: variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {TODO 'data'} { xfail *-*-* } l_data1 } */
  {
    int i;

#pragma acc kernels
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    i = 0;
  }
}
