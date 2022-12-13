/* { dg-do run { target { *-*-linux* && { ! ia32 } } } }*/
/* { dg-require-effective-target raoint }*/
/* { dg-options "-pthread -O2 -mraoint" }*/
#include "rao-helper.h"

const unsigned int num_iters= 1000000;
unsigned int thread_val[4] = { 0xffffff5a, 0xffff96ff, 0xff73ffff, 0xceffffff };
static long long shared_val = 0xffffffff;
unsigned int expected_val = 0xce73965a;

static void* 
threads_worker (state_t *tstate)
{
  int i;
  unsigned int val = thread_val[tstate->id];
  for (i = 0; i < num_iters; i++) 
    _aand_i64 (&shared_val, val);
}

static void
rao_test(void)
{
  if (shared_val != expected_val)
    abort ();	
}
