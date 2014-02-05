/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* arm*-*-* } } */
/* { dg-options "-fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* arm*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-O1" } { "" } } */

#include <assert.h>
#include <unistd.h>
#if HAVE_IO
#include <cstdio>
#include <cilk/cilk_api.h>
#endif
#include <cstdlib>


void func(int volatile* steal_me) 
{
  while (! (*steal_me)) 
    {
      usleep(2000);
    }
#if HAVE_IO
  printf("Foo executing on %d\n", __cilkrts_get_worker_number());
#endif
  throw 5;
}

void my_test() 
{
  volatile int steal_me = 0;

  try 
    {
      _Cilk_spawn func(&steal_me);
#if HAVE_IO
      printf("Continuation executing on %d\n",
	     __cilkrts_get_worker_number());
#endif
      steal_me = 1;
      _Cilk_sync;
      goto bad;
    }

  catch (int x) 
    {
#if HAVE_IO
      printf("We caught x = %d\n", x);
#endif
      assert(x == 5);
    }
  if (0) 
    {
    bad:
#if HAVE_IO
      printf("We should not be here!\n");
#endif
      __builtin_abort ();
    }
}


int main() 
{
  my_test();
#if HAVE_IO
  printf("PASSED\n");
#endif
  return 0;
}
