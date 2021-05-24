/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" } for
   testing/documenting aspects of that functionality.  */

int
reduction ()
{
  int i, r;

  #pragma acc parallel
  #pragma acc loop private (r) reduction (+:r)
  /* { dg-note {variable 'r' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} r { target *-*-* } .-1 } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} i { target *-*-* } .-2 } */
  for (i = 0; i < 100; i++)
    r += 10;

  return r;
}
