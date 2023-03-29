/* { dg-do run { target { *-*-linux* && { ! ia32 } } } }*/
/* { dg-require-effective-target raoint }*/
/* { dg-options "-pthread -O2 -mraoint" }*/
#include "rao-helper.h"

const unsigned int inc_val = 3;
const unsigned int num_iters= 1000000;
static long long shared_val = 0;

static
void* threads_worker (state_t *tstate)
{
  int i;
  for (i = 0; i < num_iters; i++) 
     _aadd_i64 (&shared_val, inc_val);
  return 0;
}

static void
rao_test (void)
{
  if (shared_val != num_iters * num_threads * inc_val)
    abort ();	
}
