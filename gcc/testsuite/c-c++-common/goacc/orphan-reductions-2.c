/* Ensure that the middle end does not assign gang level parallelism
   to orphan loop containing reductions.  */

/* { dg-do compile } */
/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-Wopenacc-parallelism" } */

#pragma acc routine gang
int
f1 () /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" } */
{
  int sum = 0, i;

#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC worker vector loop parallelism" } */
  for (i = 0; i < 100; i++)
    sum++;

  return sum;
}

#pragma acc routine gang
int
f2 () /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" } */
{
  int sum = 0, i, j;

#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
    for (j = 0; j < 100; j++)
      sum++;

  return sum;
}

#pragma acc routine gang
int
f3 () /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" } */
{
  int sum = 0, i, j, k;

#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
  for (i = 0; i < 100; i++)
#pragma acc loop reduction (+:sum) /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    /* { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } */
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
