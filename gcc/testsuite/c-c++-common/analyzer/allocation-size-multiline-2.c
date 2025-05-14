/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-fine-grained" } */

#include <stdint.h>

void test_constant_1 (void)
{
  int32_t *ptr = (int32_t *) __builtin_alloca (1); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (1);
                              ^~~~~~~~~~~~~~~~~~~~
  'test_constant_1': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (1);
                              ^~~~~~~~~~~~~~~~~~~~
                              |
                              (1) allocated 1 byte here
                              (2) assigned to 'int32_t *'
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (1);
                              ~~~~~~~~~~~~~~~~~^~~
  'void test_constant_1()': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (1);
                              ~~~~~~~~~~~~~~~~~^~~
                                               |
                                               (1) allocated 1 byte here
                                               (2) assigned to 'int32_t*' {aka '{re:long :re?}int*'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c++ } } */

void test_constant_2 (void)
{
  int32_t *ptr = (int32_t *) __builtin_alloca (2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (2);
                              ^~~~~~~~~~~~~~~~~~~~
  'test_constant_2': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (2);
                              ^~~~~~~~~~~~~~~~~~~~
                              |
                              (1) allocated 2 bytes here
                              (2) assigned to 'int32_t *'
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (2);
                              ~~~~~~~~~~~~~~~~~^~~
  'void test_constant_2()': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (2);
                              ~~~~~~~~~~~~~~~~~^~~
                                               |
                                               (1) allocated 2 bytes here
                                               (2) assigned to 'int32_t*' {aka '{re:long :re?}int*'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c++ } } */

void test_symbolic (int n)
{
  int32_t *ptr = (int32_t *) __builtin_alloca (n * 2); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
}

/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (n * 2);
                              ^~~~~~~~~~~~~~~~~~~~~~~~
  'test_symbolic': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (n * 2);
                              ^~~~~~~~~~~~~~~~~~~~~~~~
                              |
                              (1) allocated 'n * 2' bytes here
                              (2) assigned to 'int32_t *'
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   int32_t *ptr = (int32_t *) __builtin_alloca (n * 2);
                              ~~~~~~~~~~~~~~~~~^~~~~~~
  'void test_symbolic(int)': events 1-2
   int32_t *ptr = (int32_t *) __builtin_alloca (n * 2);
                              ~~~~~~~~~~~~~~~~~^~~~~~~
                                               |
                                               (1) allocated '(n * 2)' bytes here
                                               (2) assigned to 'int32_t*' {aka '{re:long :re?}int*'} here; 'sizeof (int32_t {aka {re:long :re?}int})' is '4'
   { dg-end-multiline-output "" { target c++ } } */

/* FIXME: am getting a duplicate warning here for some reason
   without -fanalyzer-fine-grained (PR PR analyzer/107851).  */

