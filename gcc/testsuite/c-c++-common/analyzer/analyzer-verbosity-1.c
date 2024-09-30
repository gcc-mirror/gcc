/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-verbosity=1" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */
/* { dg-enable-nn-line-numbers "" } */

#include <stdlib.h>

void calls_free_1 (void *ptr)
{
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_1 (void *ptr, int a, int b)
{
  if (a)
    calls_free_1 (ptr);

  if (b)
    {
    }
  else
    calls_free_1 (ptr);
}

/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test_1': events 1-2
    |
    |   NN | void test_1 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (2) calling 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': events 3-4
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (3) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (4) first 'free' here
           |
    <------+
    |
  'test_1': events 5-6
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ^~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (5) returning to 'test_1' from 'calls_free_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (6) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': events 7-8
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (7) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (8) second 'free' here; first 'free' was at (4)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_1(void*, int, int)': events 1-2
    |
    |   NN | void test_1 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |                  |
    |      |                  (2) calling 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': events 3-4
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (3) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (4) first 'free' here
           |
    <------+
    |
  'void test_1(void*, int, int)': events 5-6
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~^~~~~
    |      |                  |
    |      |                  (5) returning to 'test_1' from 'calls_free_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |                  |
    |      |                  (6) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': events 7-8
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (7) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (8) second 'free' here; first 'free' was at (4)
           |
  { dg-end-multiline-output "" { target c++ } } */

void calls_free_2 (void *ptr)
{
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_2 (void *ptr, int a, int b)
{
  switch (a)
    {
    default:
      break;
    case 1:
      break;
    case 3:
      calls_free_2 (ptr);
      break;
    }

  switch (b)
    {
    default:
      calls_free_2 (ptr);
      break;
    case 1:
      break;
    case 42:
      break;
    }
}

/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test_2': events 1-2
    |
    |   NN | void test_2 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (2) calling 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': events 3-4
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (3) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (4) first 'free' here
           |
    <------+
    |
  'test_2': events 5-6
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ^~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (5) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (6) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': events 7-8
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (7) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (8) second 'free' here; first 'free' was at (4)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_2(void*, int, int)': events 1-2
    |
    |   NN | void test_2 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (2) calling 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': events 3-4
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (3) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (4) first 'free' here
           |
    <------+
    |
  'void test_2(void*, int, int)': events 5-6
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~^~~~~
    |      |                    |
    |      |                    (5) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (6) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': events 7-8
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (7) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (8) second 'free' here; first 'free' was at (4)
           |
  { dg-end-multiline-output "" { target c++ } } */

/* The call/return to this function shouldn't appear in the path.  */

void called_by_test_3 (void)
{
}

void test_3 (void *ptr)
{
  free (ptr);
  called_by_test_3 ();
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test_3': events 1-2
   NN |   free (ptr);
      |   ^~~~~~~~~~
      |   |
      |   (1) first 'free' here
   NN |   called_by_test_3 ();
   NN |   free (ptr);
      |   ~~~~~~~~~~
      |   |
      |   (2) second 'free' here; first 'free' was at (1)
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_3(void*)': events 1-2
   NN |   free (ptr);
      |   ~~~~~^~~~~
      |        |
      |        (1) first 'free' here
   NN |   called_by_test_3 ();
   NN |   free (ptr);
      |   ~~~~~~~~~~
      |        |
      |        (2) second 'free' here; first 'free' was at (1)
  { dg-end-multiline-output "" { target c++ } } */
