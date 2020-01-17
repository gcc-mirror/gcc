/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-line-numbers -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

/* Verify the interaction of inline-events with line numbers.  */

#include <stdlib.h>

extern void missing_location ();

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

void wrapped_free (void *ptr)
{
  free (ptr); /* { dg-warning "double-free of 'ptr' \\\[CWE-415\\]" } */
  /* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test': events 1-2
    |
    |   NN | {
    |      | ^
    |      | |
    |      | (1) entering 'test'
    |   NN |   boxed_int *obj = make_boxed_int (i);
    |      |                    ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (2) calling 'make_boxed_int'
    |
    +--> 'make_boxed_int': events 3-4
           |
           |   NN | {
           |      | ^
           |      | |
           |      | (3) entering 'make_boxed_int'
           |   NN |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
           |      |                                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |                                    |
           |      |                                    (4) calling 'wrapped_malloc'
           |
           +--> 'wrapped_malloc': events 5-6
                  |
                  |   NN | {
                  |      | ^
                  |      | |
                  |      | (5) entering 'wrapped_malloc'
                  |   NN |   return malloc (size);
                  |      |          ~~~~~~~~~~~~~
                  |      |          |
                  |      |          (6) calling 'malloc'
                  |
    <-------------+
    |
  'test': event 7
    |
    |   NN |   free_boxed_int (obj);
    |      |   ^~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (7) calling 'free_boxed_int'
    |
    +--> 'free_boxed_int': events 8-9
           |
           |   NN | {
           |      | ^
           |      | |
           |      | (8) entering 'free_boxed_int'
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (9) calling 'wrapped_free'
           |
           +--> 'wrapped_free': events 10-11
                  |
                  |   NN | {
                  |      | ^
                  |      | |
                  |      | (10) entering 'wrapped_free'
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (11) calling 'free'
                  |
    <-------------+
    |
  'test': event 12
    |
    |cc1:
    | (12): calling 'missing_location'
    |
  'test': event 13
    |
    |   NN |   free_boxed_int (obj);
    |      |   ^~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (13) calling 'free_boxed_int'
    |
    +--> 'free_boxed_int': events 14-15
           |
           |   NN | {
           |      | ^
           |      | |
           |      | (14) entering 'free_boxed_int'
           |   NN |   wrapped_free (bi);
           |      |   ~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (15) calling 'wrapped_free'
           |
           +--> 'wrapped_free': events 16-17
                  |
                  |   NN | {
                  |      | ^
                  |      | |
                  |      | (16) entering 'wrapped_free'
                  |   NN |   free (ptr);
                  |      |   ~~~~~~~~~~
                  |      |   |
                  |      |   (17) calling 'free'
                  |
     { dg-end-multiline-output "" } */
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
{
  boxed_int *obj = make_boxed_int (i);

  free_boxed_int (obj);

  missing_location ();

  free_boxed_int (obj);
}

