/* { dg-do compile }  */
/* { dg-additional-options "-O2" }  */

#include "../../../../include/gomp-constants.h"

#pragma acc routine
int
foo (void)
{
  int res;
  
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

  return res;
}

void
foo2 (void)
{
  int res;

#pragma acc parallel
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
foo3 (void)
{
  int res;

#pragma acc kernels
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
