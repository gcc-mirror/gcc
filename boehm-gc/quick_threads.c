/*
 * Support code for cooperative coop/quick threads.
 * Copyright (c) 1998, 1999 Cygnus Solutions.
 */

#include "boehm-config.h"

#ifdef QUICK_THREADS

#include "gc_priv.h"

#include "coop.h"

void GC_push_all_stacks (void)
{
  coop_t *t;
  ptr_t lo, hi;

  t = coop_first_thread ();

  if (t == NULL)
    {
      /* Threads haven't started, so mark the real stack.  */
#ifdef STACK_GROWS_DOWN
      GC_push_all_stack( GC_approx_sp(), GC_stackbottom );
#else
      GC_push_all_stack( GC_stackbottom, GC_approx_sp() );
#endif
    }
  else
    {
      for ( ; t != NULL; t = coop_next_thread (t))
	{
	  if (t == coop_global_curr)
	    lo = GC_approx_sp ();
	  else
	    {
	      lo = t->top;
	      /* LO can be NULL when the new thread has not yet been
		 used.  */
	      if (! lo)
		continue;
	    }
	  hi = t->base;

#ifdef STACK_GROWS_DOWN
	  GC_push_all_stack (lo, hi);
#else
	  GC_push_all_stack (hi, lo);
#endif
	}
    }
}

#endif /* QUICK_THREADS */
