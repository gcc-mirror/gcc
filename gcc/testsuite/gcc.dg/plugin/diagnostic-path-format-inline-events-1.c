/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

#include <stdlib.h>

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

void wrapped_free (void *ptr)
{
  free (ptr); /* { dg-warning "double-free of 'ptr' \\\[CWE-415\\]" } */
  /* { dg-begin-multiline-output "" }
   free (ptr);
   ^~~~~~~~~~
  'test': events 1-2
    |
    | {
    | ^
    | |
    | (1) entering 'test'
    |   boxed_int *obj = make_boxed_int (i);
    |                    ~~~~~~~~~~~~~~~~~~
    |                    |
    |                    (2) calling 'make_boxed_int'
    |
    +--> 'make_boxed_int': events 3-4
           |
           | {
           | ^
           | |
           | (3) entering 'make_boxed_int'
           |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
           |                                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |                                    |
           |                                    (4) calling 'wrapped_malloc'
           |
           +--> 'wrapped_malloc': events 5-6
                  |
                  | {
                  | ^
                  | |
                  | (5) entering 'wrapped_malloc'
                  |   return malloc (size);
                  |          ~~~~~~~~~~~~~
                  |          |
                  |          (6) calling 'malloc'
                  |
    <-------------+
    |
  'test': event 7
    |
    |   free_boxed_int (obj);
    |   ^~~~~~~~~~~~~~~~~~~~
    |   |
    |   (7) calling 'free_boxed_int'
    |
    +--> 'free_boxed_int': events 8-9
           |
           | {
           | ^
           | |
           | (8) entering 'free_boxed_int'
           |   wrapped_free (bi);
           |   ~~~~~~~~~~~~~~~~~
           |   |
           |   (9) calling 'wrapped_free'
           |
           +--> 'wrapped_free': events 10-11
                  |
                  | {
                  | ^
                  | |
                  | (10) entering 'wrapped_free'
                  |   free (ptr);
                  |   ~~~~~~~~~~
                  |   |
                  |   (11) calling 'free'
                  |
    <-------------+
    |
  'test': event 12
    |
    |   free_boxed_int (obj);
    |   ^~~~~~~~~~~~~~~~~~~~
    |   |
    |   (12) calling 'free_boxed_int'
    |
    +--> 'free_boxed_int': events 13-14
           |
           | {
           | ^
           | |
           | (13) entering 'free_boxed_int'
           |   wrapped_free (bi);
           |   ~~~~~~~~~~~~~~~~~
           |   |
           |   (14) calling 'wrapped_free'
           |
           +--> 'wrapped_free': events 15-16
                  |
                  | {
                  | ^
                  | |
                  | (15) entering 'wrapped_free'
                  |   free (ptr);
                  |   ~~~~~~~~~~
                  |   |
                  |   (16) calling 'free'
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

  free_boxed_int (obj);
}

