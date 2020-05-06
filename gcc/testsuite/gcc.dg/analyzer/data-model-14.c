#include <stdlib.h>

void *global_ptr;

void test_1 (int i)
{
  global_ptr = malloc (1024); /* { dg-message "allocated here" } */
  *(int *)&global_ptr = i; /* { dg-warning "leak of 'global_ptr'" } */
}

void test_2 (int i)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  global_ptr = p;
  *(int *)&p = i;
  p = global_ptr;
  free (p);
  free (global_ptr); /* { dg-warning "double-'free' of 'p'" } */
}
