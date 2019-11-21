// { dg-do compile }

#include <stdlib.h>

void test_1 (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}
