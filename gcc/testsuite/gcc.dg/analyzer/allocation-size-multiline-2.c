/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-fine-grained" } */
/* { dg-require-effective-target alloca } */

#include <stdint.h>

void test_constant_1 (void)
{
  int32_t *ptr = __builtin_alloca (1); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_alloca (1);
                  ^~~~~~~~~~~~~~~~~~~~
  'test_constant_1': event 1
    |
    |   int32_t *ptr = __builtin_alloca (1);
    |                  ^~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 1 bytes and assigned to 'int32_t *' {aka 'int *'} here; 'sizeof (int32_t {aka int})' is '4'
    |
   { dg-end-multiline-output "" } */

void test_constant_2 (void)
{
  int32_t *ptr = __builtin_alloca (2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_alloca (2);
                  ^~~~~~~~~~~~~~~~~~~~
  'test_constant_2': event 1
    |
    |   int32_t *ptr = __builtin_alloca (2);
    |                  ^~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 2 bytes and assigned to 'int32_t *' {aka 'int *'} here; 'sizeof (int32_t {aka int})' is '4'
    |
   { dg-end-multiline-output "" } */

void test_symbolic (int n)
{
  int32_t *ptr = __builtin_alloca (n * 2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = __builtin_alloca (n * 2);
                  ^~~~~~~~~~~~~~~~~~~~~~~~
  'test_symbolic': event 1
    |
    |   int32_t *ptr = __builtin_alloca (n * 2);
    |                  ^~~~~~~~~~~~~~~~~~~~~~~~
    |                  |
    |                  (1) allocated 'n * 2' bytes and assigned to 'int32_t *' {aka 'int *'} here; 'sizeof (int32_t {aka int})' is '4'
    |
   { dg-end-multiline-output "" } */

/* FIXME: am getting a duplicate warning here for some reason
   without -fanalyzer-fine-grained (PR PR analyzer/107851).  */

