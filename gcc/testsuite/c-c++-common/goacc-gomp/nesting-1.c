/* { dg-additional-options "--param=openacc-kernels=decompose" }

/* { dg-additional-options "-fopt-info-omp-note" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */


void
f_acc_data (void)
{
#pragma acc data
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 } */
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}

void
f_acc_kernels (void)
{
#pragma acc kernels
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 } */
  {
    int i;
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} "" { target c } .+3 }
       { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} "" { target c++ } .+1 } */
#pragma omp atomic write
    i = 0;
  }
}

#pragma acc routine vector
void
f_acc_loop (void)
{
  int i;

#pragma acc loop
  /* { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  /* { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 }
     { dg-bogus {note: variable 'i' ought to be adjusted for OpenACC privatization level: 'UNKNOWN'} "TODO" { xfail *-*-* } .-3 } */
  for (i = 0; i < 2; ++i)
    {
#pragma omp atomic write
      i = 0;
    }
}

void
f_acc_parallel (void)
{
#pragma acc parallel
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 }
     { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } .-2 } */
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}

void
f_acc_serial (void)
{
#pragma acc serial
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 }
     { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } .-2 } */
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}
