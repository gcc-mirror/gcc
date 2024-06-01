/* Verify that we report events that occur at returns from functions
   with valid-looking interprocedural paths.

   C only: there are only minor C/C++ differences in e.g. how the
   function names are printed.  Trying to handle both would overcomplicate
   the test.  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */
/* { dg-additional-options "-fdiagnostics-show-path-depths" } */

#include <stdlib.h>
#include <stdint.h>

void *create_buffer (int32_t n)
{
  return malloc(n);
}

void test_9 (void) 
{
  int32_t *buf = (int32_t *) create_buffer(42); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  free (buf);
}

/* { dg-begin-multiline-output "" }
   NN |   int32_t *buf = (int32_t *) create_buffer(42);
      |                              ^~~~~~~~~~~~~~~~~
  'test_9': events 1-2 (depth 1)
    |
    |   NN | void test_9 (void)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_9'
    |   NN | {
    |   NN |   int32_t *buf = (int32_t *) create_buffer(42);
    |      |                              ~~~~~~~~~~~~~~~~~
    |      |                              |
    |      |                              (2) calling 'create_buffer' from 'test_9'
    |
    +--> 'create_buffer': events 3-4 (depth 2)
           |
           |   NN | void *create_buffer (int32_t n)
           |      |       ^~~~~~~~~~~~~
           |      |       |
           |      |       (3) entry to 'create_buffer'
           |   NN | {
           |   NN |   return malloc(n);
           |      |          ~~~~~~~~~
           |      |          |
           |      |          (4) allocated 42 bytes here
           |
    <------+
    |
  'test_9': event 5 (depth 1)
    |
    |   NN |   int32_t *buf = (int32_t *) create_buffer(42);
    |      |                              ^~~~~~~~~~~~~~~~~
    |      |                              |
    |      |                              (5) assigned to 'int32_t *'
    |
   { dg-end-multiline-output "" } */
