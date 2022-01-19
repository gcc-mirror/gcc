/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-g -fcompare-debug" } -- w/ debug compiled first.
   { dg-bogus {error: [^\n\r]+: '-fcompare-debug' failure \(length\)} TODO { xfail c++ } 0 } */
/* { dg-additional-options "-O1" } so that we may get some 'GIMPLE_DEBUG's.  */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

extern int i;

void
f_acc_kernels (void)
{
  /* { dg-bogus {sorry, unimplemented: 'gimple_debug' not yet supported} {} { target *-*-* } .+1 } */
#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-note {variable 'i\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  {
    /* { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {w/ debug} { xfail c++ } .-1 } */

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_i1 } */
    /* { dg-note {variable 'i\.0' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i1 } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i1 } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i1 } */
    for (i = 0; i < 2; ++i)
      ;
  }
}
