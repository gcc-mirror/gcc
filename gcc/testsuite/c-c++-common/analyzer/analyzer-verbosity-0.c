/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-verbosity=0" } */
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
  'test_1': event 1
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ^~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (1) calling 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': event 2
           |
           |   NN |   free (ptr);
           |      |   ^~~~~~~~~~
           |      |   |
           |      |   (2) first 'free' here
           |
    <------+
    |
  'test_1': events 3-4
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ^~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (3) returning to 'test_1' from 'calls_free_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (4) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': event 5
           |
           |   NN |   free (ptr);
           |      |   ^~~~~~~~~~
           |      |   |
           |      |   (5) second 'free' here; first 'free' was at (2)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_1(void*, int, int)': event 1
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~^~~~~
    |      |                  |
    |      |                  (1) calling 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': event 2
           |
           |   NN |   free (ptr);
           |      |   ~~~~~^~~~~
           |      |        |
           |      |        (2) first 'free' here
           |
    <------+
    |
  'void test_1(void*, int, int)': events 3-4
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~^~~~~
    |      |                  |
    |      |                  (3) returning to 'test_1' from 'calls_free_1'
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |                  |
    |      |                  (4) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': event 5
           |
           |   NN |   free (ptr);
           |      |   ~~~~~^~~~~
           |      |        |
           |      |        (5) second 'free' here; first 'free' was at (2)
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
  'test_2': event 1
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ^~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (1) calling 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': event 2
           |
           |   NN |   free (ptr);
           |      |   ^~~~~~~~~~
           |      |   |
           |      |   (2) first 'free' here
           |
    <------+
    |
  'test_2': events 3-4
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ^~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (3) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (4) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': event 5
           |
           |   NN |   free (ptr);
           |      |   ^~~~~~~~~~
           |      |   |
           |      |   (5) second 'free' here; first 'free' was at (2)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_2(void*, int, int)': event 1
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~^~~~~
    |      |                    |
    |      |                    (1) calling 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': event 2
           |
           |   NN |   free (ptr);
           |      |   ~~~~~^~~~~
           |      |        |
           |      |        (2) first 'free' here
           |
    <------+
    |
  'void test_2(void*, int, int)': events 3-4
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~^~~~~
    |      |                    |
    |      |                    (3) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (4) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': event 5
           |
           |   NN |   free (ptr);
           |      |   ~~~~~^~~~~
           |      |        |
           |      |        (5) second 'free' here; first 'free' was at (2)
           |
  { dg-end-multiline-output "" { target c++ } } */

// TODO: range cases

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
