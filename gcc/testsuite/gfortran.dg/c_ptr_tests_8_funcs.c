/* This file provides auxiliary functions for c_ptr_tests_8.  */

#include <stdio.h>
#include <stdlib.h>

extern void abort (void);

void *create (void)
{
  int *a;
  a = malloc (sizeof (a));
  *a = 444;
  return a;

}

void show (int *a)
{
  if (*a == 444)
    printf ("SUCCESS (%d)\n", *a);
  else
  {
    printf ("FAILED: Expected 444, received %d\n", *a);
    abort ();
  }
}
