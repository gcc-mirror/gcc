/* Must define:
   int expect_error;
   void test ();
   void setup () NOCHECK; */

#include "driver.h"

int expect_error = 0;

int *ip;

void test ()
{
  ip = c_malloc (sizeof (int));
  t2 (ip);
}

int t2 (int *ip)
{
}

void setup () /* NOCHECK */
{
  mark_region (&ip, sizeof (ip), ACCESS_RW);
}
