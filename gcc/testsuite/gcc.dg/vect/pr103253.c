/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-additional-options "-O2 -fexceptions -fopenmp -fno-delete-dead-exceptions -fno-trapping-math" } */

double
do_work (double do_work_pri)
{
  int i;

#pragma omp simd
  for (i = 0; i < 17; ++i)
    do_work_pri = (!i ? 0.5 : i) * 2.0;

  return do_work_pri;
}

