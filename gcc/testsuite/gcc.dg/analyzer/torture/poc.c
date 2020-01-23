/* { dg-do link } */

#include <stdlib.h>

void test_1 (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-warning "double-free" } */
}

struct s
{
  void *ptr;
};

void test_2 (struct s *x)
{
  free (x->ptr);
  free (x->ptr); /* { dg-warning "double-free" } */
}

/* TODO: be more precise about what is freed.  */

int main () {}
