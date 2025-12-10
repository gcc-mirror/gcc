/* { dg-do compile } */
/* { dg-options "-Wall" } */
/* { dg-require-fork "" } */

/* Compile with -Wall to get a warning if built-in and system pid_t don't
   match.  */

#include <sys/types.h>

typedef __typeof (__builtin_fork ()) __builtin_pid_t;

__builtin_pid_t __p_t__;
pid_t *p_t_p;

void
pt (void)
{
  p_t_p = &__p_t__;
}
