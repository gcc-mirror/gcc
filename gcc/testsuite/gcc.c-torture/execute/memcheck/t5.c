/* Must define:
   int expect_error;
   void test ();
   void setup () NOCHECK; */

#include "driver.h"

int expect_error = 1;

struct s {
  char c;
  int a, b;
};

struct s *sp;

void test ()
{
  sp = c_malloc (sizeof (struct s));
  sp->c = 0;
  sp->b = 47;
  foo (sp);
}

int foo (struct s *sp)
{
  return sp->c + sp->a + sp->b;
}

void setup () /* NOCHECK */
{
  mark_region (&sp, sizeof (sp), ACCESS_RW);
}
