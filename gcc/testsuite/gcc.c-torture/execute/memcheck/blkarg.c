/* Must define:
   int expect_error;
   void test ();
   void setup () NOCHECK; */

#include "driver.h"

/* Test permissions of BLKmode arguments constructed purely on the
   stack.

   Maybe we can't guarantee that we'll always wind up with stack args,
   but if we don't, they're in registers, and permissions should just
   always yield success.  So while this test may not be effective on
   all platforms, failure probably does indicate a real bug.

   Note that because of the implementation, we do want to test BLKmode
   arguments that live purely on the stack and are constructed there.
   We want to test other situations of function arguments, of course,
   but don't assume this case would be covered by using one monster
   argument that is read from memory (including using constructor
   syntax but constant values), or may live partially in registers.  */

int expect_error = 0;

/* Must be BLKmode.  Using only two fields gets TImode on Alpha.  */
struct S
{
  unsigned long long ll;
  long               xx;
  long               yy;
};

unsigned long long   x = 0x12345689ULL;
#define I2	     42

static int first_time = 1;

/* Leading ten arguments force struct S onto the stack on both Alpha and MIPS.  */
int
foo (int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9, int a10,
     struct S s)
{
  if (a1 != 1 || a2 != 2 || a3 != 3 || a4 != 4 || a5 != 5 || a6 != 6 || a7 != 7
      || a8 != 8 || a9 !=9 || a10 != 10)
    abort ();
  
  if (first_time)
    {
      if (s.ll != x || s.xx != I2 || s.yy != 0)
	abort ();
      
      first_time = 0;
    }
  else
    {
      if (s.ll != 0 || s.xx != 0 || s.yy != 0)
	abort ();
    }
  
  return 0;
}

void
test ()
{
  foo (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, (struct S) { x, I2 });
  foo (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, (struct S) { 0 });
}

void
setup () /* NOCHECK */
{
  mark_region (&x, sizeof (x), ACCESS_RO);
  mark_region (&first_time, sizeof (first_time), ACCESS_RW);
}
