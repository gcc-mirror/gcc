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

#pragma acc loop deux /* { dg-error "expected '#pragma acc' clause before 'deux'" } */
  for (i = 0; i < 2; ++i)
    ;
}
