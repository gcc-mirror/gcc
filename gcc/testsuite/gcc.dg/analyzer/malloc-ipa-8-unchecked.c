/* Example of a multilevel wrapper around malloc, with an unchecked write.  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fanalyzer-checker=malloc -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include <stdlib.h>

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

typedef struct boxed_int
{
  int i;
} boxed_int;

boxed_int *
make_boxed_int (int i)
{
  boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
  result->i = i; /* { dg-warning "dereference of possibly-NULL 'result'" } */
  return result;
}

/* "dereference of possibly-NULL 'result' [CWE-690]".  */
/* { dg-begin-multiline-output "" }
   NN |   result->i = i;
      |   ~~~~~~~~~~^~~
  'make_boxed_int': events 1-2
    |
    |   NN | make_boxed_int (int i)
    |      | ^~~~~~~~~~~~~~
    |      | |
    |      | (1) entry to 'make_boxed_int'
    |   NN | {
    |   NN |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
    |      |                                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |      |                                    |
    |      |                                    (2) calling 'wrapped_malloc' from 'make_boxed_int'
    |
    +--> 'wrapped_malloc': events 3-4
           |
           |   NN | void *wrapped_malloc (size_t size)
           |      |       ^~~~~~~~~~~~~~
           |      |       |
           |      |       (3) entry to 'wrapped_malloc'
           |   NN | {
           |   NN |   return malloc (size);
           |      |          ~~~~~~~~~~~~~
           |      |          |
           |      |          (4) this call could return NULL
           |
    <------+
    |
  'make_boxed_int': events 5-6
    |
    |   NN |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
    |      |                                    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |      |                                    |
    |      |                                    (5) possible return of NULL to 'make_boxed_int' from 'wrapped_malloc'
    |   NN |   result->i = i;
    |      |   ~~~~~~~~~~~~~                     
    |      |             |
    |      |             (6) 'result' could be NULL: unchecked value from (4)
    |
  { dg-end-multiline-output "" } */
