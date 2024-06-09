/* Verify that we can disable analyzer warnings via pragmas.  */

#include <stdlib.h>

void test_1 (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-warning "double-'free'" } */
}

void test_2 (void *ptr)
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wanalyzer-double-free\"")

  free (ptr);
  free (ptr);  /* { dg-bogus "double-'free'" } */

  _Pragma("GCC diagnostic pop")
}

void test_3 (void *ptr)
{
  free (ptr);
  free (ptr);  /* { dg-warning "double-'free'" } */
}
