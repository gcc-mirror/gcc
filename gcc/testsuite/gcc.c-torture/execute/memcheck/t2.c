/* Must define:
   int expect_error;
   void test ();
   void setup () NOCHECK; */

#include "driver.h"

int expect_error = 1;

int *ip;

void test ()
{
  ip = c_malloc (sizeof (int));
  t2 ();
}

int t2 ()
{
  return *ip;
}

void setup () /* NOCHECK */
{
  mark_region (&ip, sizeof (ip), ACCESS_RW);
}
