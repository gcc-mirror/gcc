/* { dg-do run } */
/* { dg-require-effective-target sync_longlong } */
/* { dg-require-effective-target pthread_h } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-pthread -std=gnu99" } */

/* test of long long atomic ops performed in parallel in 3 pthreads
   david.gilbert@linaro.org */

#include <pthread.h>
#include <unistd.h>

/*#define DEBUGIT 1 */

#ifdef DEBUGIT
#include <stdio.h>

#define DOABORT(x,...) {\
	 fprintf (stderr, x, __VA_ARGS__); fflush (stderr); abort ();\
	 }

#else

#define DOABORT(x,...) abort ();

#endif

/* Passed to each thread to describe which bits it is going to work on.  */
struct threadwork {
  unsigned long long count; /* incremented each time the worker loops.  */
  unsigned int thread;    /* ID */
  unsigned int addlsb;    /* 8 bit */
  unsigned int logic1lsb; /* 5 bit */
  unsigned int logic2lsb; /* 8 bit */
};

/* The shared word where all the atomic work is done.  */
static volatile long long workspace;

/* A shared word to tell the workers to quit when non-0.  */
static long long doquit;

extern void abort (void);

/* Note this test doesn't test the return values much.  */
void*
worker (void* data)
{
  struct threadwork *tw = (struct threadwork*)data;
  long long add1bit = 1ll << tw->addlsb;
  long long logic1bit = 1ll << tw->logic1lsb;
  long long logic2bit = 1ll << tw->logic2lsb;

  /* Clear the bits we use.  */
  __sync_and_and_fetch (&workspace, ~(0xffll * add1bit));
  __sync_fetch_and_and (&workspace, ~(0x1fll * logic1bit));
  __sync_fetch_and_and (&workspace, ~(0xffll * logic2bit));

  do
    {
      long long tmp1, tmp2, tmp3;
      /* OK, lets try and do some stuff to the workspace - by the end
         of the main loop our area should be the same as it is now - i.e. 0.  */

      /* Push the arithmetic section upto 128 - one of the threads will
         case this to carry accross the 32bit boundary.  */
      for (tmp2 = 0; tmp2 < 64; tmp2++)
	{
	  /* Add 2 using the two different adds.  */
	  tmp1 = __sync_add_and_fetch (&workspace, add1bit);
	  tmp3 = __sync_fetch_and_add (&workspace, add1bit);

	  /* The value should be the intermediate add value in both cases.  */
	  if ((tmp1 & (add1bit * 0xff)) != (tmp3 & (add1bit * 0xff)))
	    DOABORT ("Mismatch of add intermediates on thread %d "
			"workspace=0x%llx tmp1=0x%llx "
			"tmp2=0x%llx tmp3=0x%llx\n",
			 tw->thread, workspace, tmp1, tmp2, tmp3);
	}

      /* Set the logic bits.  */
      tmp2=__sync_or_and_fetch (&workspace,
			  0x1fll * logic1bit | 0xffll * logic2bit);

      /* Check the logic bits are set and the arithmetic value is correct.  */
      if ((tmp2 & (0x1fll * logic1bit | 0xffll * logic2bit
			| 0xffll * add1bit))
	  != (0x1fll * logic1bit | 0xffll * logic2bit | 0x80ll * add1bit))
	DOABORT ("Midloop check failed on thread %d "
			"workspace=0x%llx tmp2=0x%llx "
			"masktmp2=0x%llx expected=0x%llx\n",
		tw->thread, workspace, tmp2,
		tmp2 & (0x1fll * logic1bit | 0xffll * logic2bit |
			 0xffll * add1bit),
		(0x1fll * logic1bit | 0xffll * logic2bit | 0x80ll * add1bit));

      /* Pull the arithmetic set back down to 0 - again this should cause a
	 carry across the 32bit boundary in one thread.  */

      for (tmp2 = 0; tmp2 < 64; tmp2++)
	{
	  /* Subtract 2 using the two different subs.  */
	  tmp1=__sync_sub_and_fetch (&workspace, add1bit);
	  tmp3=__sync_fetch_and_sub (&workspace, add1bit);

	  /* The value should be the intermediate sub value in both cases.  */
	  if ((tmp1 & (add1bit * 0xff)) != (tmp3 & (add1bit * 0xff)))
	    DOABORT ("Mismatch of sub intermediates on thread %d "
			"workspace=0x%llx tmp1=0x%llx "
			"tmp2=0x%llx tmp3=0x%llx\n",
			tw->thread, workspace, tmp1, tmp2, tmp3);
	}


      /* Clear the logic bits.  */
      __sync_fetch_and_xor (&workspace, 0x1fll * logic1bit);
      tmp3=__sync_and_and_fetch (&workspace, ~(0xffll * logic2bit));

      /* The logic bits and the arithmetic bits should be zero again.  */
      if (tmp3 & (0x1fll * logic1bit | 0xffll * logic2bit | 0xffll * add1bit))
	DOABORT ("End of worker loop; bits none 0 on thread %d "
			"workspace=0x%llx tmp3=0x%llx "
			"mask=0x%llx maskedtmp3=0x%llx\n",
		tw->thread, workspace, tmp3, (0x1fll * logic1bit |
			0xffll * logic2bit | 0xffll * add1bit),
		tmp3 & (0x1fll * logic1bit | 0xffll * logic2bit | 0xffll * add1bit));

      __sync_add_and_fetch (&tw->count, 1);
    }
  while (!__sync_bool_compare_and_swap (&doquit, 1, 1));

  pthread_exit (0);
}

int
main ()
{
  /* We have 3 threads doing three sets of operations, an 8 bit
     arithmetic field, a 5 bit logic field and an 8 bit logic
     field (just to pack them all in).

  6      5       4       4       3       2       1
  3      6       8       0       2       4       6       8       0
  |...,...|...,...|...,...|...,...|...,...|...,...|...,...|...,...
  - T0   --  T1  -- T2   --T2 --  T0  -*- T2-- T1-- T1   -***- T0-
   logic2  logic2  arith   log2  arith  log1 log1  arith     log1

  */
  unsigned int t;
  long long tmp;
  int err;

  struct threadwork tw[3]={
    { 0ll, 0, 27, 0, 56 },
    { 0ll, 1,  8,16, 48 },
    { 0ll, 2, 40,21, 35 }
  };

  pthread_t threads[3];

  __sync_lock_release (&doquit);

  /* Get the work space into a known value - All 1's.  */
  __sync_lock_release (&workspace); /* Now all 0.  */
  tmp = __sync_val_compare_and_swap (&workspace, 0, -1ll);
  if (tmp!=0)
    DOABORT ("Initial __sync_val_compare_and_swap wasn't 0 workspace=0x%llx "
		"tmp=0x%llx\n", workspace,tmp);

  for (t = 0; t < 3; t++)
  {
    err=pthread_create (&threads[t], NULL , worker, &tw[t]);
    if (err) DOABORT ("pthread_create failed on thread %d with error %d\n",
	t, err);
  };

  sleep (5);

  /* Stop please.  */
  __sync_lock_test_and_set (&doquit, 1ll);

  for (t = 0; t < 3; t++)
    {
      err=pthread_join (threads[t], NULL);
      if (err)
	DOABORT ("pthread_join failed on thread %d with error %d\n", t, err);
    };

  __sync_synchronize ();

  /* OK, so all the workers have finished -
     the workers should have zero'd their workspace, the unused areas
     should still be 1.  */
  if (!__sync_bool_compare_and_swap (&workspace, 0x040000e0ll, 0))
    DOABORT ("End of run workspace mismatch, got %llx\n", workspace);

  /* All the workers should have done some work.  */
  for (t = 0; t < 3; t++)
    {
      if (tw[t].count == 0) DOABORT ("Worker %d gave 0 count\n", t);
    };

  return 0;
}

