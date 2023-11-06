/* { dg-do run } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-std=c99 -pthread" } */

/* https://github.com/llvm/llvm-project/pull/67391#issuecomment-1752403934
   reported that this had failed with GCC and 3A6000.  */

#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>

static unsigned int tags[32];
static unsigned int vals[32];

static void *
writer_entry (void *data)
{
  atomic_uint *pt = (atomic_uint *)tags;
  atomic_uint *pv = (atomic_uint *)vals;

  for (unsigned int n = 1; n < 10000; n++)
    {
      atomic_store_explicit (&pv[n & 31], n, memory_order_release);
      atomic_store_explicit (&pt[n & 31], n, memory_order_release);
    }

  return NULL;
}

static void *
reader_entry (void *data)
{
  atomic_uint *pt = (atomic_uint *)tags;
  atomic_uint *pv = (atomic_uint *)vals;
  int i;

  for (;;)
    {
      for (i = 0; i < 32; i++)
        {
          unsigned int tag = 0;
          bool res;

          res = atomic_compare_exchange_weak_explicit (
              &pt[i], &tag, 0, memory_order_acquire, memory_order_acquire);
          if (!res)
            {
              unsigned int val;

              val = atomic_load_explicit (&pv[i], memory_order_relaxed);
              if (val < tag)
                __builtin_trap ();
            }
        }
    }

  return NULL;
}

int
main (int argc, char *argv[])
{
  pthread_t writer;
  pthread_t reader;
  int res;

  res = pthread_create (&writer, NULL, writer_entry, NULL);
  if (res < 0)
    __builtin_trap ();

  res = pthread_create (&reader, NULL, reader_entry, NULL);
  if (res < 0)
    __builtin_trap ();

  res = pthread_join (writer, NULL);
  if (res < 0)
    __builtin_trap ();

  return 0;
}
