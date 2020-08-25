/* Example of a multilevel wrapper around malloc/free, with a double-'free'.  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fanalyzer-checker=malloc -fdiagnostics-show-caret" } */
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
    +--> 'make_boxed_int': events 3-4
           |
           |   NN | make_boxed_int (int i)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (3) entry to 'make_boxed_int'
           |   NN | {
           |   NN |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
           |      |                                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |                                    |
           |      |                                    (4) calling 'wrapped_malloc' from 'make_boxed_int'
           |
           +--> 'wrapped_malloc': events 5-6
                  |
                  |   NN | void *wrapped_malloc (size_t size)
                  |      |       ^~~~~~~~~~~~~~
                  |      |       |
                  |      |       (5) entry to 'wrapped_malloc'
                  |   NN | {
                  |   NN |   return malloc (size);
                  |      |          ~~~~~~~~~~~~~
                  |      |          |
                  |      |          (6) allocated here
                  |
           <------+
           |
         'make_boxed_int': events 7-10
           |
           |   NN |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
           |      |                                    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |                                    |
           |      |                                    (7) returning to 'make_boxed_int' from 'wrapped_malloc'
           |   NN |   if (!result)
           |      |      ~                              
           |      |      |
           |      |      (8) assuming 'result' is non-NULL
           |      |      (9) following 'false' branch (when 'result' is non-NULL)...
           |   NN |     abort ();
           |   NN |   result->i = i;
           |      |   ~~~~~~~~~~~~~                     
           |      |             |
           |      |             (10) ...to here
           |
    <------+
    |
  'test': events 11-12
    |
    |   NN |   boxed_int *obj = make_boxed_int (i);
    |      |                    ^~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (11) returning to 'test' from 'make_boxed_int'
    |   NN | 
    |   NN |   free_boxed_int (obj);
    |      |   ~~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (12) calling 'free_boxed_int' from 'test'
    |
    +--> 'free_boxed_int': events 13-14
           |
           |   NN | free_boxed_int (boxed_int *bi)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (13) entry to 'free_boxed_int'
           |   NN | {
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (14) calling 'wrapped_free' from 'free_boxed_int'
           |
           +--> 'wrapped_free': events 15-16
                  |
                  |   NN | void wrapped_free (void *ptr)
                  |      |      ^~~~~~~~~~~~
                  |      |      |
                  |      |      (15) entry to 'wrapped_free'
                  |   NN | {
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (16) first 'free' here
                  |
           <------+
           |
         'free_boxed_int': event 17
           |
           |   NN |   wrapped_free (bi);
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (17) returning to 'free_boxed_int' from 'wrapped_free'
           |
    <------+
    |
  'test': events 18-19
    |
    |   NN |   free_boxed_int (obj);
    |      |   ^~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (18) returning to 'test' from 'free_boxed_int'
    |   NN | 
    |   NN |   free_boxed_int (obj);
    |      |   ~~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (19) passing freed pointer 'obj' in call to 'free_boxed_int' from 'test'
    |
    +--> 'free_boxed_int': events 20-21
           |
           |   NN | free_boxed_int (boxed_int *bi)
           |      | ^~~~~~~~~~~~~~
           |      | |
           |      | (20) entry to 'free_boxed_int'
           |   NN | {
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (21) passing freed pointer 'bi' in call to 'wrapped_free' from 'free_boxed_int'
           |
           +--> 'wrapped_free': events 22-23
                  |
                  |   NN | void wrapped_free (void *ptr)
                  |      |      ^~~~~~~~~~~~
                  |      |      |
                  |      |      (22) entry to 'wrapped_free'
                  |   NN | {
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (23) second 'free' here; first 'free' was at (16)
                  |
   { dg-end-multiline-output "" } */

/* TODO: the event describing the allocation is uninteresting and probably
   should be purged.  */
