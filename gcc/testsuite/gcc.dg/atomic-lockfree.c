/* Test __atomic routines for existence and execution with each valid 
   memory model.  */
/* { dg-options "-w" } */
/* { dg-do run } */
/* { dg-additional-sources "atomic-lockfree-aux.c" } */

/* Test that __atomic_{is,always}_lock_free builtins execute.
   sync-mem-lockfree-aux.c supplies and external entry point for 
   __atomic_is_lock_free which always returns a 2. We can detect the 
   external routine was called if 2 is returned since that is not a valid
   result normally.  */

#include <stdlib.h>

extern void abort();

int r1, r2;

/* Test for consistency on sizes 1, 2, 4, 8, 16 and 32.  */
int
main ()
{
  
  r1 = __atomic_always_lock_free (sizeof(char), 0);
  r2 = __atomic_is_lock_free (sizeof(char), 0);
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }
  
  r1 = __atomic_always_lock_free (2, 0);
  r2 = __atomic_is_lock_free (2, 0);
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }

   
  r1 = __atomic_always_lock_free (4, 0);
  r2 = __atomic_is_lock_free (4, 0);     /* Try passing in a variable.  */
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }

   
  r1 = __atomic_always_lock_free (8, 0);
  r2 = __atomic_is_lock_free (8, 0);
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }

   
  r1 = __atomic_always_lock_free (16, 0);
  r2 = __atomic_is_lock_free (16, 0);
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }

   
  r1 = __atomic_always_lock_free (32, 0);
  r2 = __atomic_is_lock_free (32, 0);
  /* If always lock free, then is_lock_free must also be true.  */
  if (r1)
    { 
      if (r2 != 1)  
	abort ();
    }
  else
    {
      /* If it is not lock free, then the external routine must be called.  */
      if (r2 != 2) 
	abort ();
    }

 
  return 0;
}

