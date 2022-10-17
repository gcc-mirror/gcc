/* Verify that we diagnose "gang reduction on an orphan loop" for automatically
   assigned gang level of parallelism.  */

/* { dg-do compile } */
/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-Wopenacc-parallelism" } */

#pragma acc routine gang
/* { dg-bogus "warning: region is worker partitioned but does not contain worker partitioned code" "TODO default 'gang' 'vector'" { xfail *-*-* } .+3 }
   TODO It's the compiler's own decision to not use 'worker' parallelism here, so it doesn't make sense to bother the user about it.  */
int
f1 ()
{
  int sum = 0, i;

  /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang vector loop parallelism" } */
  for (i = 0; i < 100; i++)
    sum++;

  return sum;
}

#pragma acc routine gang
int
f2 ()
{
  int sum = 0, i, j;

  /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang worker loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
    for (j = 0; j < 100; j++)
      sum++;

  return sum;
}

#pragma acc routine gang
int
f3 ()
{
  int sum = 0, i, j, k;

  /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
    for (j = 0; j < 100; j++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
      for (k = 0; k < 100; k++)
	sum++;

  return sum;
}

int
main ()
{
  int sum = 0, i, j, k;

#pragma acc parallel copy (sum)
  {
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang vector loop parallelism" } */
  for (i = 0; i < 100; i++)
    sum++;
  }

#pragma acc parallel copy (sum)
  {
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang worker loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
    for (j = 0; j < 100; j++)
      sum++;
  }

#pragma acc parallel copy (sum)
  {
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC gang loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
    for (j = 0; j < 100; j++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
      for (k = 0; k < 100; k++)
	sum++;
  }

  return sum;
}
