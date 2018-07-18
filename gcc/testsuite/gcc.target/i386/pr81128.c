/* PR ipa/81128 */
/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-ifunc "" } */


#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int resolver_fn = 0;
int resolved_fn = 0;

static inline void
do_it_right_at_runtime_A ()
{
  resolved_fn++;
}

static inline void
do_it_right_at_runtime_B ()
{
  resolved_fn++;
}

static inline void do_it_right_at_runtime (void);

void do_it_right_at_runtime (void)
  __attribute__ ((ifunc ("resolve_do_it_right_at_runtime")));

static void (*resolve_do_it_right_at_runtime (void)) (void)
{
  srand (time (NULL));
  int r = rand ();
  resolver_fn++;

  /* Use intermediate variable to get a warning for non-matching
   * prototype. */
  typeof(do_it_right_at_runtime) *func;
  if (r & 1)
    func = do_it_right_at_runtime_A;
  else
    func = do_it_right_at_runtime_B;

  return (void *) func;
}

int
main (void)
{
  const unsigned int ITERS = 10;

  for (int i = ITERS; i > 0; i--)
    {
      do_it_right_at_runtime ();
    }

  if (resolver_fn != 1)
    __builtin_abort ();

  if (resolved_fn != 10)
    __builtin_abort ();

  return 0;
}
