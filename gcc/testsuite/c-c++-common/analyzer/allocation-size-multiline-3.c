/* Verify that we warn for incorrect uses of "alloca" (which may be in a 
   macro in a system header), and that the output looks correct.  */

/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-fine-grained" } */

#include <stdint.h>
#include "../../gcc.dg/analyzer/test-alloca.h"

void test_constant_99 (void)
{
  int32_t *ptr = (int32_t *) alloca (99); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) alloca (99);
                              ^~~~~~
  'test_constant_99': events 1-2
   int32_t *ptr = (int32_t *) alloca (99);
                              ^~~~~~
                              |
                              (1) allocated 99 bytes here
                              (2) assigned to 'int32_t *' {aka '{re:long :re?}int *'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) alloca (99);
                              ^~~~~~
  'void test_constant_99()': events 1-2
   int32_t *ptr = (int32_t *) alloca (99);
                              ^~~~~~
                              |
                              (1) allocated 99 bytes here
                              (2) assigned to 'int32_t*' {aka '{re:long :re?}int*'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c++ } } */

void test_symbolic (int n)
{
  int32_t *ptr = (int32_t *) alloca (n * 2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) alloca (n * 2);
                              ^~~~~~
  'test_symbolic': events 1-2
   int32_t *ptr = (int32_t *) alloca (n * 2);
                              ^~~~~~
                              |
                              (1) allocated 'n * 2' bytes here
                              (2) assigned to 'int32_t *' {aka '{re:long :re?}int *'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) alloca (n * 2);
                              ^~~~~~
  'void test_symbolic(int)': events 1-2
   int32_t *ptr = (int32_t *) alloca (n * 2);
                              ^~~~~~
                              |
                              (1) allocated '(n * 2)' bytes here
                              (2) assigned to 'int32_t*' {aka '{re:long :re?}int*'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c++ } } */
