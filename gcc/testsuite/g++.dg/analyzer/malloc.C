// { dg-do compile }
/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void test_1 (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

/* Test of double-free in ctor.  */

struct s2
{
  s2 (void *v)
  {
    free (v); // { dg-warning "double-'free' of 'v'" }
  }
};

void test_2 (void *ptr)
{
  free (ptr); // { dg-message "first 'free' here" }
  s2 a (ptr); // { dg-message "passing freed pointer 'ptr' in call to 's2::s2' from 'test_2'" }
}
