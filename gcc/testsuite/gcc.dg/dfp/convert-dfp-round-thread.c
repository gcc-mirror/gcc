/* { dg-options "-std=gnu99 -D_GNU_SOURCE -pthread" } */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* } } */

/* N1150 5.2: Conversions among decimal floating types and between
   decimal floating types and generic floating types.
   C99 6.3.1.5(3) New.

   Perform conversions between DFP types in which the assigned value
   cannot be represented exactly in the result and must be rounded
   correctly according to the current rounding mode.

   Normally this would not be part of compiler testing, but conversions
   are currently handled in libgcc via decNumber.  */

#include <pthread.h>
#include <error.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include "dfp-round.h"

extern void abort (void);
static __thread int failcnt = 0;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

pthread_mutex_t mut1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mut2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mut3 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mut4 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mut5 = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond1 = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond2 = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond3 = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond4 = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond5 = PTHREAD_COND_INITIALIZER;
pthread_barrier_t bar1;
pthread_barrier_t bar2;
pthread_barrier_t bar3;
pthread_barrier_t bar4;
pthread_barrier_t bar5;

__thread _Decimal32 d32;
__thread _Decimal64 d64;
__thread _Decimal128 d128;

_Decimal32 d64_to_d32 (_Decimal64 d) { return d; }
_Decimal64 d128_to_d64 (_Decimal128 d) { return d; }
_Decimal32 d128_to_d32 (_Decimal128 d) { return d; }

int
do_d64_to_d32 (_Decimal64 orig, _Decimal32 exp)
{
  d64 = orig;
  d32 = d64_to_d32 (d64);
  return (d32 == exp);
}

int
do_d128_to_d32 (_Decimal128 orig, _Decimal32 exp)
{
  d128 = orig;
  d32 = d128_to_d32 (d128);
  return (d32 == exp);
}

int
do_d128_to_d64 (_Decimal128 orig, _Decimal64 exp)
{
  d128 = orig;
  d64 = d128_to_d64 (d128);
  return (d64 == exp);
}

void *
downward (void *arg)
{
  int err;

  DFP_SETROUND (FE_DEC_DOWNWARD);

  err = pthread_mutex_lock (&mut1);
  if (err != 0)
    error (EXIT_FAILURE, err, "downward: failed to lock");

  err = pthread_barrier_wait (&bar1);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("downward: barrier_wait failed");
      exit (1);
    } 

  err = pthread_cond_wait (&cond1, &mut1);
  if (err != 0)
    error (EXIT_FAILURE, err, "downward: failed to wait");

  err = pthread_mutex_unlock (&mut1);
  if (err != 0)
    error (EXIT_FAILURE, err, "downward: failed to unlock");

  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE
  
#ifdef DBG
  if (failcnt)
    printf ("downward: %d fails\n", failcnt);
#endif
  return (void *) (ptrdiff_t) failcnt;
}

void *
tonearest (void *arg)
{
  int err;
  DFP_SETROUND (FE_DEC_TONEAREST);

  err = pthread_mutex_lock (&mut2);
  if (err != 0)
    error (EXIT_FAILURE, err, "tonearest: failed to lock");

  err = pthread_barrier_wait (&bar2);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("tonearest: barrier_wait failed");
      exit (1);
    } 

  err = pthread_cond_wait (&cond2, &mut2);
  if (err != 0)
    error (EXIT_FAILURE, err, "tonearest: failed to wait");

  err = pthread_mutex_unlock (&mut2);
  if (err != 0)
    error (EXIT_FAILURE, err, "tonearest: failed to unlock");

  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE
  
#ifdef DBG
  if (failcnt)
    printf ("tonearest: %d fails\n", failcnt);
#endif
  return (void *) (ptrdiff_t) failcnt;
} 

void *
toneareastfromzero (void *arg)
{
  int err;
  DFP_SETROUND (FE_DEC_TONEARESTFROMZERO);

  err = pthread_mutex_lock (&mut3);
  if (err != 0)
    error (EXIT_FAILURE, err, "toneareastfromzero: failed to lock");

  err = pthread_barrier_wait (&bar3);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("toneareastfromzero: barrier_wait failed");
      exit (1);
    } 

  err = pthread_cond_wait (&cond3, &mut3);
  if (err != 0)
    error (EXIT_FAILURE, err, "toneareastfromzero: failed to wait");

  err = pthread_mutex_unlock (&mut3);
  if (err != 0)
    error (EXIT_FAILURE, err, "toneareastfromzero: failed to unlock");

  if (!do_d64_to_d32 (1.1111125dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE
  
#ifdef DBG
  if (failcnt)
    printf ("toneareastfromzero: %d fails\n", failcnt);
#endif
  return (void *) (ptrdiff_t) failcnt;
} 
  
void *
towardzero (void *arg)
{
  int err;
  DFP_SETROUND (FE_DEC_TOWARDZERO);

  err = pthread_mutex_lock (&mut4);
  if (err != 0)
    error (EXIT_FAILURE, err, "towardzero: failed to lock");

  err = pthread_barrier_wait (&bar4);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("towardzero: barrier_wait failed");
      exit (1);
    } 

  err = pthread_cond_wait (&cond4, &mut4);
  if (err != 0)
    error (EXIT_FAILURE, err, "towardzero: failed to wait");

  err = pthread_mutex_unlock (&mut4);
  if (err != 0)
    error (EXIT_FAILURE, err, "towardzero: failed to unlock");

  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111113df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111113dd)) FAILURE

#ifdef DBG
  if (failcnt)
    printf ("towardzero: %d fails\n", failcnt);
#endif
  return (void *) (ptrdiff_t) failcnt;
} 

void *
upward (void *arg)
{
  int err;
  DFP_SETROUND (FE_DEC_UPWARD);

  err = pthread_mutex_lock (&mut5);
  if (err != 0)
    error (EXIT_FAILURE, err, "upward: failed to lock");

  err = pthread_barrier_wait (&bar5);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("upward: barrier_wait failed");
      exit (1);
    } 

  err = pthread_cond_wait (&cond5, &mut5);
  if (err != 0)
    error (EXIT_FAILURE, err, "upward: failed to wait");

  err = pthread_mutex_unlock (&mut5);
  if (err != 0)
    error (EXIT_FAILURE, err, "upward: failed to unlock");

  if (!do_d64_to_d32 (1.1111125dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111113df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111113dd)) FAILURE
  
#ifdef DBG
  if (failcnt)
    printf ("upward: %d fails\n", failcnt);
#endif
  return (void *) (ptrdiff_t) failcnt;
}

int
main (void)
{
  int err;
  int count = 0;
  void *ret;
  pthread_t down, up, tozero, fromzero, nearest;

  if (pthread_barrier_init (&bar1, NULL, 2) != 0
      || pthread_barrier_init (&bar2, NULL, 2) != 0
      || pthread_barrier_init (&bar3, NULL, 2) != 0
      || pthread_barrier_init (&bar4, NULL, 2) != 0
      || pthread_barrier_init (&bar5, NULL, 2) != 0)
    {
      puts ("parent: failed to init barrier");
      return 1;
    }

  if (pthread_create (&down, NULL, downward, NULL) != 0)
    {
      puts ("parent: failed to create");
      return 1;
    }

  if (pthread_create (&nearest, NULL, tonearest, NULL) != 0)
    {
      puts ("create failed");
      return 1;
    }

  if (pthread_create (&fromzero, NULL, toneareastfromzero, NULL) != 0)
    {
      puts ("create failed");
      return 1;
    }

  if (pthread_create (&up, NULL, upward, NULL) != 0)
    {
      puts ("create failed");
      return 1;
    }

  if (pthread_create (&tozero, NULL, towardzero, NULL) != 0)
    {
      puts ("create failed");
      return 1;
    }

  err = pthread_barrier_wait (&bar1);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("parent: failed to wait barrier 1");
      return 1;
    }
  err = pthread_barrier_wait (&bar2);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("parent: failed to wait barrier 2");
      return 1;
    }
  err = pthread_barrier_wait (&bar3);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("parent: failed to wait barrier 3");
      return 1;
    }
  err = pthread_barrier_wait (&bar4);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("parent: failed to wait barrier 4");
      return 1;
    }
  err = pthread_barrier_wait (&bar5);
  if (err != 0 && err != PTHREAD_BARRIER_SERIAL_THREAD)
    {
      puts ("parent: failed to wait barrier 5");
      return 1;
    }

  err = pthread_mutex_lock (&mut1);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: lock failed");
  err = pthread_mutex_lock (&mut2);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: lock failed");
  err = pthread_mutex_lock (&mut3);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: lock failed");
  err = pthread_mutex_lock (&mut4);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: lock failed");
  err = pthread_mutex_lock (&mut5);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: lock failed");

  err = pthread_cond_signal(&cond1);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: broadcast failed");
  err = pthread_cond_signal(&cond2);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: broadcast failed");
  err = pthread_cond_signal(&cond3);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: broadcast failed");
  err = pthread_cond_signal(&cond4);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: broadcast failed");
  err = pthread_cond_signal(&cond5);
  if (err != 0)
    error (EXIT_FAILURE, err, "parent: broadcast failed");

  err = pthread_mutex_unlock (&mut1);
  if (err != 0)
    {
      puts ("parent: failed to unlock");
      return 1;
    }
  err = pthread_mutex_unlock (&mut2);
  if (err != 0)
    {
      puts ("parent: failed to unlock");
      return 1;
    }
  err = pthread_mutex_unlock (&mut3);
  if (err != 0)
    {
      puts ("parent: failed to unlock");
      return 1;
    }
  err = pthread_mutex_unlock (&mut4);
  if (err != 0)
    {
      puts ("parent: failed to unlock");
      return 1;
    }
  err = pthread_mutex_unlock (&mut5);
  if (err != 0)
    {
      puts ("parent: failed to unlock");
      return 1;
    }

  if (pthread_join (down, &ret) != 0)
    {
      puts ("pthread_join failed");
      return 1;
    }
  count += (int) (ptrdiff_t) ret;

  if (pthread_join (up, &ret) != 0)
    {
      puts ("pthread_join failed");
      return 1;
    }
  count += (int) (ptrdiff_t) ret;

  if (pthread_join (tozero, &ret) != 0)
    {
      puts ("pthread_join failed");
      return 1;
    }
  count += (int) (ptrdiff_t) ret;

  if (pthread_join (fromzero, &ret) != 0)
    {
      puts ("pthread_join failed");
      return 1;
    }
  count += (int) (ptrdiff_t) ret;

  if (pthread_join (nearest, &ret) != 0)
    {
      puts ("pthread_join failed");
      return 1;
    }
  count += (int) (ptrdiff_t) ret;

  if (count)
    {
#ifdef DBG
      printf ("Total: %d fails\n", count);
#endif
      abort ();
    }

  return 0;
}
