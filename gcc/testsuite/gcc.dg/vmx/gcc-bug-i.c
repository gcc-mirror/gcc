#include "harness.h"

/* This problem occurs if a function is inlined.  When its local
   variables get allocated space on the caller's (the function to
   which it is inlined) stack frame, they don't get 16-byte alignment
   even if they need it.  Here's an example with a union (that's the
   first case I uncovered, but it's probably a general occurrence on
   inlining).  */

#define N 10
/* adjust N = size of buffer to try to get bad alignment for inlined union */

#define DO_INLINE __attribute__ ((always_inline))
#define DONT_INLINE __attribute__ ((noinline))

static DO_INLINE int inline_me(vector signed short data) 
{
  union {vector signed short v; signed short s[8];} u;
  u.v = data;
  return u.s[7];
}

static DONT_INLINE int foo(vector signed short data)
{
  int c, buffer[N], i;
  c = inline_me(data);
  for (i=0; i<N; i++) {
    if (i == 0)
      buffer[i] = c;
    else
      buffer[i] = buffer[i-1] + c*i;
  }
  return buffer[N-1];
}

static void test()
{
  check(foo((vector signed short)
	    ((vector unsigned char){1,2,3,4,5,6,7,8,
				   9,10,11,12,13,14,15,16})) == 0x2b4e0,
	"foo");
}
