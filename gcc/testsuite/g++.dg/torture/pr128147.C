// { dg-do run { target { { *-*-linux* *-*-gnu* } && { mmap && signal } } } }
// { dg-additional-options "-fnon-call-exceptions -fno-delete-dead-exceptions" }

#include <signal.h>
#include <stdlib.h>
#include <sys/mman.h>

static void
on_segv (int)
{
  throw 1;
}

// Load p from *pp first so that the trapping load *p is not the first
// instruction of f.  Unwinders that look up a signal frame with PC - 2
// (ARM EHABI) would otherwise find the preceding function.

__attribute__((noipa)) int
f (int **pp, int c)
{
  int *p = *pp;
  int x = *p;
  if (c)
    return x;
  return 0;
}

int
main ()
{
  int *p = (int *) mmap (0, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS,
			 -1, 0);
  if (p == MAP_FAILED || signal (SIGSEGV, on_segv) == SIG_ERR)
    return 0;
  try
    {
      f (&p, 0);
    }
  catch (int)
    {
      return 0;
    }
  abort ();
}
