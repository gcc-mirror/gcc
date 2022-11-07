/* { dg-do run { target { *-*-linux* && { ! ia32 } } } }*/
/* { dg-require-effective-target raoint }*/
/* { dg-options "-pthread -O2 -mraoint" }*/
#include "rao-helper.h"

const unsigned int num_iters= 1000001;
unsigned int thread_val[4] = { 0x5a, 0x9600, 0x730000, 0xce000000 };
static long long shared_val = 0;
unsigned int expected_val = 0xce73965a;

static void* 
threads_worker (state_t *tstate)
{
  int i;
  unsigned int val = thread_val[tstate->id];
  for (i = 0; i < num_iters; i++) 
    _axor_i64 (&shared_val, val);
}

static void
rao_test (void)
{
  if (shared_val != expected_val)
    abort ();	
}
