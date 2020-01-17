/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=separate-events" } */

#include <stdlib.h>

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

void wrapped_free (void *ptr)
{
  free (ptr); /* { dg-warning "double-free of 'ptr' \\\[CWE-415\\]" } */
}

typedef struct boxed_int
{
  int i;
} boxed_int;

boxed_int *
make_boxed_int (int i)
{
  boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
  result->i = i;
  return result;
}

void
free_boxed_int (boxed_int *bi)
{
  wrapped_free (bi);
}

void test (int i)
{ /* { dg-message "\\(1\\) entering 'test'" } */
  boxed_int *obj = make_boxed_int (i); /* { dg-message "\\(2\\) calling 'make_boxed_int'" } */
  /* etc */

  free_boxed_int (obj);

  free_boxed_int (obj);
}

