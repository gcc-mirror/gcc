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
  sp = c_malloc (sizeof (struct s) * 2);
  sp->c = 0;
  sp->b = 47;
  cp (sp);
  foo (sp);
}

int foo (struct s *sp)
{
  return sp[1].c + sp[1].a + sp[1].b;
}

int cp (struct s *sp)
{
  sp[1] = sp[0];
}

void setup () /* NOCHECK */
{
  mark_region (&sp, sizeof (sp), ACCESS_RW);
}
