/* Miscellaneous tests where clause parsing is expected to fail.  */

void
f (void)
{
  int i;

#pragma acc parallel one /* { dg-error "expected '#pragma acc' clause before 'one'" } */
  ;

#pragma acc kernels eins /* { dg-error "expected '#pragma acc' clause before 'eins'" } */
  ;

#pragma acc data two /* { dg-error "expected '#pragma acc' clause before 'two'" } */
  ;

#pragma acc parallel
#pragma acc loop deux /* { dg-error "expected '#pragma acc' clause before 'deux'" } */
  for (i = 0; i < 2; ++i)
    ;
}


void
f2 (void)
{
  int a, b[100];

#pragma acc parallel firstprivate (b[10:20]) /* { dg-error "expected ... before ... token" } */
  ;
}
