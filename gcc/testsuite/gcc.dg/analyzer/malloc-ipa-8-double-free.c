/* Example of a multilevel wrapper around malloc/free, with a double-'free'.  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fanalyzer-checker=malloc -fanalyzer-verbose-state-changes -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include <stdlib.h>

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

void wrapped_free (void *ptr)
{
  free (ptr); /* { dg-warning "double-'free' of 'ptr' \\\[CWE-415\\\]" } */
}

typedef struct boxed_int
{
  int i;
} boxed_int;

boxed_int *
make_boxed_int (int i)
{
  boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
  if (!result)
    abort ();
  result->i = i;
  return result;
}

void
free_boxed_int (boxed_int *bi)
{
  wrapped_free (bi);
}

void test (int i)
{
  boxed_int *obj = make_boxed_int (i);

  free_boxed_int (obj);

  free_boxed_int (obj);
}

/* double-'free'.  */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test': events 1-2
    |
    |   NN | void test (int i)
    |      |      ^~~~
    |      |      |
    |      |      (1) entry to 'test'
    |   NN | {
    |   NN |   boxed_int *obj = make_boxed_int (i);
    |      |                    ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (2) calling 'make_boxed_int' from 'test'
    |
    +--> 'make_boxed_int': events 3-6
           |
           |   NN | make_boxed_int (int i)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (3) entry to 'make_boxed_int'
           |......
           |   NN |   if (!result)
           |      |      ~
           |      |      |
           |      |      (4) following 'false' branch (when 'result' is non-NULL)...
           |   NN |     abort ();
           |   NN |   result->i = i;
           |      |   ~~~~~~~~~~~~~
           |      |             |
           |      |             (5) ...to here
           |   NN |   return result;
           |      |          ~~~~~~
           |      |          |
           |      |          (6) state of '<return-value>': 'start' -> 'nonnull' (origin: NULL)
           |
    <------+
    |
  'test': events 7-8
    |
    |   NN |   boxed_int *obj = make_boxed_int (i);
    |      |                    ^~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (7) returning to 'test' from 'make_boxed_int'
    |   NN | 
    |   NN |   free_boxed_int (obj);
    |      |   ~~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (8) calling 'free_boxed_int' from 'test'
    |
    +--> 'free_boxed_int': events 9-10
           |
           |   NN | free_boxed_int (boxed_int *bi)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (9) entry to 'free_boxed_int'
           |   NN | {
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (10) calling 'wrapped_free' from 'free_boxed_int'
           |
           +--> 'wrapped_free': events 11-12
                  |
                  |   NN | void wrapped_free (void *ptr)
                  |      |      ^~~~~~~~~~~~
                  |      |      |
                  |      |      (11) entry to 'wrapped_free'
                  |   NN | {
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (12) first 'free' here (state of 'ptr': 'nonnull' -> 'freed', origin: NULL)
                  |
           <------+
           |
         'free_boxed_int': event 13
           |
           |   NN |   wrapped_free (bi);
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (13) returning to 'free_boxed_int' from 'wrapped_free'
           |
    <------+
    |
  'test': events 14-15
    |
    |   NN |   free_boxed_int (obj);
    |      |   ^~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (14) returning to 'test' from 'free_boxed_int'
    |   NN | 
    |   NN |   free_boxed_int (obj);
    |      |   ~~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (15) passing freed pointer 'obj' in call to 'free_boxed_int' from 'test'
    |
    +--> 'free_boxed_int': events 16-17
           |
           |   NN | free_boxed_int (boxed_int *bi)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (16) entry to 'free_boxed_int'
           |   NN | {
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (17) passing freed pointer 'bi' in call to 'wrapped_free' from 'free_boxed_int'
           |
           +--> 'wrapped_free': events 18-19
                  |
                  |   NN | void wrapped_free (void *ptr)
                  |      |      ^~~~~~~~~~~~
                  |      |      |
                  |      |      (18) entry to 'wrapped_free'
                  |   NN | {
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (19) second 'free' here; first 'free' was at (12) ('ptr' is in state 'freed')
                  |
   { dg-end-multiline-output "" } */

/* TODO: the event describing the allocation is uninteresting and probably
   should be purged.  */
