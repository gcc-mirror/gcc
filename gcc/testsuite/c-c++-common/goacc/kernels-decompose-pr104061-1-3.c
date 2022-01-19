/* { dg-additional-options "--param openacc-kernels=decompose" } */

/* { dg-additional-options "-fchecking" }
   { dg-ice TODO }
   { dg-prune-output {D\.[0-9]+ = arr_0\.0 \+ k;} }
   { dg-prune-output {during GIMPLE pass: lower} } */

/* { dg-additional-options "-fcompare-debug" } -- w/o debug compiled first.
   { dg-bogus {error: during '-fcompare-debug' recompilation} TODO { xfail *-*-* } 0 }
   { dg-bogus {error: [^\n\r]+: '-fcompare-debug' failure \(length\)} TODO { xfail *-*-* } 0 } */
/* { dg-additional-options "-O1" } so that we may get some 'GIMPLE_DEBUG's.  */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

int arr_0;

void
foo (void)
{
  /* { dg-bogus {sorry, unimplemented: 'gimple_debug' not yet supported} {} { target *-*-* } .+1 } suppressed via '-fcompare-debug'.  */
#pragma acc kernels /* { dg-line l_compute1 } */
  /* { dg-bogus {note: variable 'k' declared in block is candidate for adjusting OpenACC privatization level} {w/ debug} { xfail *-*-* } l_compute1 } */
  /* { dg-note {variable 'arr_0\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 } */
  {
    /* { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {w/ debug} { xfail c++ } .-1 }
       { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {w/ debug} { xfail c } .+1 } */
    int k;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_k1 } */
    /* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k1 } */
    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k1 } */
    /* { dg-bogus {note: variable 'k' in 'private' clause is candidate for adjusting OpenACC privatization level} {w/ debug} { xfail *-*-* } l_loop_k1 } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_k1 } */
    for (k = 0; k < 2; k++)
      arr_0 += k;
      /* { dg-bogus {error: invalid operands in binary operation} {w/ debug} { xfail *-*-* } .-1 } */
  }
}
