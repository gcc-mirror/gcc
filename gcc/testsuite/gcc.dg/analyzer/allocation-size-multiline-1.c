/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

#include <stdint.h>

void test_constant_1 (void)
{
  int32_t *ptr = __builtin_malloc (1); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  __builtin_free (ptr);
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_malloc (1);
                  ^~~~~~~~~~~~~~~~~~~~
  'test_constant_1': events 1-2
    |
    |   int32_t *ptr = __builtin_malloc (1);
    |                  ^~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 1 byte here
    |                  (2) assigned to 'int32_t *'
    |
   { dg-end-multiline-output "" } */

void test_constant_2 (void)
{
  int32_t *ptr = __builtin_malloc (2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  __builtin_free (ptr);
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_malloc (2);
                  ^~~~~~~~~~~~~~~~~~~~
  'test_constant_2': events 1-2
    |
    |   int32_t *ptr = __builtin_malloc (2);
    |                  ^~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 2 bytes here
    |                  (2) assigned to 'int32_t *'
    |
   { dg-end-multiline-output "" } */

void test_symbolic (int n)
{
  int32_t *ptr = __builtin_malloc (n * 2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  __builtin_free (ptr);
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_malloc (n * 2);
                  ^~~~~~~~~~~~~~~~~~~~~~~~
  'test_symbolic': event 1
    |
    |   int32_t *ptr = __builtin_malloc (n * 2);
    |                  ^~~~~~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 'n * 2' bytes and assigned to 'int32_t *'
    |
   { dg-end-multiline-output "" } */
