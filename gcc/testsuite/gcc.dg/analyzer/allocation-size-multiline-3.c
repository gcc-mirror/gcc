/* Verify that we warn for incorrect uses of "alloca" (which may be in a 
   macro in a system header), and that the output looks correct.  */

/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-fine-grained" } */
/* { dg-require-effective-target alloca } */

#include <stdint.h>
#include "test-alloca.h"

void test_constant_99 (void)
{
  int32_t *ptr = alloca (99); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = alloca (99);
                  ^~~~~~
  'test_constant_99': event 1
    |
    |   int32_t *ptr = alloca (99);
    |                  ^~~~~~
    |                  |
    |                  (1) allocated 99 bytes and assigned to 'int32_t *' {aka 'int *'} here; 'sizeof (int32_t {aka int})' is '4'
    |
   { dg-end-multiline-output "" } */

void test_symbolic (int n)
{
  int32_t *ptr = alloca (n * 2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = alloca (n * 2);
                  ^~~~~~
  'test_symbolic': event 1
    |
    |   int32_t *ptr = alloca (n * 2);
    |                  ^~~~~~
    |                  |
    |                  (1) allocated 'n * 2' bytes and assigned to 'int32_t *' {aka 'int *'} here; 'sizeof (int32_t {aka int})' is '4'
    |
   { dg-end-multiline-output "" } */
