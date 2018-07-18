/* { dg-do run } */
/* tree inline used to split the block for inlining after the call,
   then move the call to the after-the-call block.  This move
   temporarily deletes the assignment to the result, which in turn
   resets any debug bind stmts referencing the result.  Make sure we
   don't do that, verifying that the result is visible after the call,
   and when passed to another inline function.  */
/* { dg-options "-g" } */

#define GUALITY_DONT_FORCE_LIVE_AFTER -1

#ifndef STATIC_INLINE
#define STATIC_INLINE /*static*/
#endif


#include "guality.h"

__attribute__ ((always_inline)) static inline int
t1 (int i)
{
  GUALCHKVAL (i);
  return i;
}
__attribute__ ((always_inline)) static inline int
t2 (int i)
{
  GUALCHKVAL (i);
  return i - 42;
}
int
main (int argc, char *argv[])
{
  int i = t1(42);
  GUALCHKVAL (i);
  return t2(i);
}
