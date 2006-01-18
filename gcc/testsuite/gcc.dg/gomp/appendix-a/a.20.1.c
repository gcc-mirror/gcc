/* { dg-do compile } */

void
a20_wrong ()
{
  int a = 1;
#pragma omp parallel
  {
    if (a != 0)
#pragma omp flush(a)	/* { dg-error "'#pragma omp flush' may only" } */
/* incorrect as flush cannot be immediate substatement
    of if statement */
      if (a != 0)
#pragma omp barrier	/* { dg-error "'#pragma omp barrier' may only" } */
/* incorrect as barrier cannot be immediate substatement
    of if statement */
  }
}
