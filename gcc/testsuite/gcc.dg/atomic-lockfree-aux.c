/* Test supply a __atomic_is_lock_free routine for lock-free tests.  */
/* Just compile it on its own.  */
/* { dg-do compile } */
/* { dg-options "-w" } */

/* Test that __atomic_{is,always}_lock_free builtins execute.  */

#include <stdlib.h>

/* Supply a builtin external function which returns a non-standard value so
   it can be detected that it was called.  */
int 
__atomic_is_lock_free (size_t s, void *p)
{
  return 2;
}

