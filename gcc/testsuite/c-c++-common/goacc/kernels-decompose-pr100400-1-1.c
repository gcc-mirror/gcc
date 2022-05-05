/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-g0" } */
/* { dg-additional-options "-O1" } */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

int *p;

void
foo (void)
{
#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'p' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute1 }
     { dg-note {variable 'p' made addressable} {} { target *-*-* } l_compute1 } */
  /* { dg-note {variable 'c' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute1 } */
  /* { dg-note {variable 'c\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  {
    int c;

    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    p = &c;

    /* { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
#pragma acc loop independent /* { dg-line l_loop_c1 } */
    /* { dg-note {variable 'c\.0' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c1 } */
    /* { dg-note {variable 'c' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_c1 }
       { dg-note {variable 'c' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop_c1 } */
    /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_c1 } */
    for (c = 0; c < 1; ++c)
      ;
  }
}
