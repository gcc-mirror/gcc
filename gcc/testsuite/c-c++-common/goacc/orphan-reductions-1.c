/* Test orphan reductions.  */

/* { dg-do compile } */

#pragma acc routine seq
int
seq_reduction (int n)
{
  int i, sum = 0;
#pragma acc loop seq reduction(+:sum)
  for (i = 0; i < n; i++)
    sum = sum + 1;

  return sum;
}

#pragma acc routine gang
int
gang_reduction (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc loop gang reduction(+:s1) /* { dg-error "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc loop gang reduction(+:s2) /* { dg-error "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;


  return s1 + s2;
}

#pragma acc routine worker
int
worker_reduction (int n)
{
  int i, sum = 0;
#pragma acc loop worker reduction(+:sum)
  for (i = 0; i < n; i++)
    sum = sum + 3;

  return sum;
}

#pragma acc routine vector
int
vector_reduction (int n)
{
  int i, sum = 0;
#pragma acc loop vector reduction(+:sum)
  for (i = 0; i < n; i++)
    sum = sum + 4;

  return sum;
}
