/* Must define:
   int expect_error;
   void test ();
   void setup () NOCHECK; */

#include "driver.h"

int expect_error = 1;

typedef struct {
  short a;
  char b;
} S1;
typedef struct {
  struct { int x; S1 *s1p; } *p;
} S2;

S1 *s1;
S2 *s2;

void test ()
{
  s1 = c_malloc (sizeof (S1));
  s2 = c_malloc (sizeof (S2));
  s2->p = c_malloc (sizeof (*s2->p));
  s2->p->s1p = s1;
  s1->a = 47;
  foo ();
}

int foo ()
{
  return s2->p->s1p->b;
}

void setup () /* NOCHECK */
{
  mark_region (&s1, sizeof (s1), ACCESS_RW);
  mark_region (&s2, sizeof (s2), ACCESS_RW);
}
