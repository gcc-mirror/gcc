/* { dg-do compile }  */
/* { dg-additional-options "-O2" }  */

#include "../../../../include/gomp-constants.h"

void
foo (void)
{
  __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  /* { dg-error "'__builtin_goacc_parlevel_id' only supported in OpenACC code" "" { target *-*-* } .-1 } */
  
  __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
  /* { dg-error "'__builtin_goacc_parlevel_size' only supported in OpenACC code" "" { target *-*-* } .-1 } */
}

#pragma acc routine
void
foo2 (int arg)
{
  __builtin_goacc_parlevel_id (arg);
  /* { dg-error "non-constant argument 0 to '__builtin_goacc_parlevel_id'" "" { target *-*-* } .-1 } */

  __builtin_goacc_parlevel_size (arg);
  /* { dg-error "non-constant argument 0 to '__builtin_goacc_parlevel_size'" "" { target *-*-* } .-1 } */

  __builtin_goacc_parlevel_id (-1);
  /* { dg-error "illegal argument 0 to '__builtin_goacc_parlevel_id'" "" { target *-*-* } .-1 } */

  __builtin_goacc_parlevel_id (-1);
  /* { dg-error "illegal argument 0 to '__builtin_goacc_parlevel_id'" "" { target *-*-* } .-1 } */

  __builtin_goacc_parlevel_size (-1);
  /* { dg-error "illegal argument 0 to '__builtin_goacc_parlevel_size'" "" { target *-*-* } .-1 } */

  __builtin_goacc_parlevel_size (3);
  /* { dg-error "illegal argument 0 to '__builtin_goacc_parlevel_size'" "" { target *-*-* } .-1 } */
}
