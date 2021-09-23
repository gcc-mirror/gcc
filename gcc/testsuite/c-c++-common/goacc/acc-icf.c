/* Ensure that IPA-ICF is disabled on OpenACC routines.  */

/* { dg-additional-options "-fopenacc -O2 -fdump-ipa-icf" }  */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#pragma acc routine gang
/* { dg-bogus "warning: region is worker partitioned but does not contain worker partitioned code" "TODO default 'gang' 'vector'" { xfail *-*-* } .+3 }
   TODO It's the compiler's own decision to not use 'worker' parallelism here, so it doesn't make sense to bother the user about it.  */
int
routine1 (int n)
{
  int i;

  #pragma acc loop
  for (i = 0; i < n; i++)
    ;

  return n + 1;
}

#pragma acc routine gang
/* { dg-bogus "warning: region is worker partitioned but does not contain worker partitioned code" "TODO default 'gang' 'vector'" { xfail *-*-* } .+3 }
   TODO It's the compiler's own decision to not use 'worker' parallelism here, so it doesn't make sense to bother the user about it.  */
int
routine2 (int n)
{
  int i;

  #pragma acc loop
  for (i = 0; i < n; i++)
    ;

  return n + 1;
}

int
main ()
{
  int i;

  #pragma acc parallel loop
  for (i = 0; i < 8; i++)
    ;

  #pragma acc parallel loop
  for (i = 0; i < 8; i++)
    ;

  return 0;
}

/* { dg-final { scan-ipa-dump-times "with total: 1 items" 5 "icf" } }  */
