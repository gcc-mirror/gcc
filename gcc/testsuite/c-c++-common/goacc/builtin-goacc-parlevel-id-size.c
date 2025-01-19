/* { dg-do compile }  */
/* { dg-additional-options "-O2" }  */

/* { dg-additional-options -Wuninitialized } */

#include "../../../../include/gomp-constants.h"

#pragma acc routine
int
foo_routine (void)
{
  int res;
  /* { dg-note {'res' was declared here} {} { target *-*-* } .-1 } */
  
  __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

  __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
  __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
  __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);

  res += __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  /* { dg-warning {'res' is used uninitialized} {} { target *-*-* } .-1 } */
  res += __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  res += __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

  res += __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
  res += __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
  res += __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);

  return res;
}

void
foo_parallel (void)
{
  int res;

#pragma acc parallel
  /* implicit 'firstprivate (res)'
     { dg-warning {'res' is used uninitialized} TODO { xfail *-*-* } .-2 } */
  {
    __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }
}

void
foo_kernels (void)
{
  int res;

#pragma acc kernels
  /* implicit 'copy (res)'
     { dg-warning {'res' is used uninitialized} TODO { xfail *-*-* } .-2 } */
  {
    __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }
}

void
foo_serial (void)
{
  int res;

#pragma acc serial
  /* implicit 'firstprivate (res)'
     { dg-warning {'res' is used uninitialized} TODO { xfail *-*-* } .-2 } */
  {
    __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

    res += __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    res += __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }
}
