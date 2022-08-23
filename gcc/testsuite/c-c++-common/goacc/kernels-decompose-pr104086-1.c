/* Reduced from 'libgomp.oacc-c-c++-common/declare-vla.c', and then
   extended.  */

/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

void
foo (void)
{
#pragma acc data /* { dg-line l_data1 } */
  /* { dg-bogus {note: variable 'i' declared in block is candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } l_data1 } */
  {
    int i;

#pragma acc kernels /* { dg-line l_compute1 } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute1 }
       { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute1 } */
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    i = 0;

#pragma acc kernels /* { dg-line l_compute2 } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute2 }
       { dg-note {variable 'i' already made addressable} {} { target *-*-* } l_compute2 } */
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    i = -1;
  }
}

void
foo2 (void)
{
  int i[1];

#pragma acc kernels /* { dg-line l2_compute1 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l2_compute1 }
     { dg-note {variable 'i' made addressable} {} { target *-*-* } l2_compute1 } */
  /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
  i[0] = 0;

#pragma acc kernels /* { dg-line l2_compute2 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l2_compute2 }
     { dg-note {variable 'i' already made addressable} {} { target *-*-* } l2_compute2 } */
  /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
  i[0] = -1;
}
