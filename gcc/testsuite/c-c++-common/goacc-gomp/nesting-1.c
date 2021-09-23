/* { dg-additional-options "-fopt-info-omp-note" } */
/* { dg-additional-options "--param=openacc-privatization=noisy" } for
   testing/documenting aspects of that functionality.  */


void
f_acc_data (void)
{
#pragma acc data
  /* { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 } */
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
  /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 }
     { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } .-2 } */
  {
    int i;
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
