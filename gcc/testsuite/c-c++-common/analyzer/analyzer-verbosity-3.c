/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fanalyzer-verbosity=3" } */
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
  'test_1': events 1-4
    |
    |   NN | void test_1 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_1'
    |   NN | {
    |   NN |   if (a)
    |      |      ~
    |      |      |
    |      |      (2) following 'true' branch (when 'a != 0')...
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (3) ...to here
    |      |     (4) calling 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': events 5-6
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (5) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (6) first 'free' here
           |
    <------+
    |
  'test_1': events 7-10
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ^~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (7) returning to 'test_1' from 'calls_free_1'
    |   NN | 
    |   NN |   if (b)
    |      |      ~
    |      |      |
    |      |      (8) following 'false' branch (when 'b == 0')...
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (9) ...to here
    |      |     (10) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'calls_free_1': events 11-12
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (11) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (12) second 'free' here; first 'free' was at (6)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_1(void*, int, int)': events 1-4
    |
    |   NN | void test_1 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_1'
    |   NN | {
    |   NN |   if (a)
    |      |   ~~  
    |      |   |
    |      |   (2) following 'true' branch (when 'a != 0')...
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |                  |
    |      |                  (3) ...to here
    |      |                  (4) calling 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': events 5-6
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (5) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (6) first 'free' here
           |
    <------+
    |
  'void test_1(void*, int, int)': events 7-10
    |
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~^~~~~
    |      |                  |
    |      |                  (7) returning to 'test_1' from 'calls_free_1'
    |   NN | 
    |   NN |   if (b)
    |      |   ~~              
    |      |   |
    |      |   (8) following 'false' branch (when 'b == 0')...
    |......
    |   NN |     calls_free_1 (ptr);
    |      |     ~~~~~~~~~~~~~~~~~~
    |      |                  |
    |      |                  (9) ...to here
    |      |                  (10) passing freed pointer 'ptr' in call to 'calls_free_1' from 'test_1'
    |
    +--> 'void calls_free_1(void*)': events 11-12
           |
           |   NN | void calls_free_1 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (11) entry to 'calls_free_1'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (12) second 'free' here; first 'free' was at (6)
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
  'test_2': events 1-4
    |
    |   NN | void test_2 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_2'
    |   NN | {
    |   NN |   switch (a)
    |      |   ~~~~~~
    |      |   |
    |      |   (2) following 'case 3:' branch...
    |......
    |   NN |     case 3:
    |      |     ~~~~
    |      |     |
    |      |     (3) ...to here
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (4) calling 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': events 5-6
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (5) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (6) first 'free' here
           |
    <------+
    |
  'test_2': events 7-10
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ^~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (7) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |   switch (b)
    |      |   ~~~~~~
    |      |   |
    |      |   (8) following 'default:' branch...
    |   NN |     {
    |   NN |     default:
    |      |     ~~~~~~~
    |      |     |
    |      |     (9) ...to here
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (10) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'calls_free_2': events 11-12
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (11) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |   |
           |      |   (12) second 'free' here; first 'free' was at (6)
           |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ~~~~~^~~~~
  'void test_2(void*, int, int)': events 1-4
    |
    |   NN | void test_2 (void *ptr, int a, int b)
    |      |      ^~~~~~
    |      |      |
    |      |      (1) entry to 'test_2'
    |   NN | {
    |   NN |   switch (a)
    |      |   ~~~~~~
    |      |   |
    |      |   (2) following 'case 3:' branch...
    |......
    |   NN |     case 3:
    |      |     ~~~~
    |      |     |
    |      |     (3) ...to here
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (4) calling 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': events 5-6
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (5) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (6) first 'free' here
           |
    <------+
    |
  'void test_2(void*, int, int)': events 7-10
    |
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~^~~~~
    |      |                    |
    |      |                    (7) returning to 'test_2' from 'calls_free_2'
    |......
    |   NN |   switch (b)
    |      |   ~~~~~~            
    |      |   |
    |      |   (8) following 'default:' branch...
    |   NN |     {
    |   NN |     default:
    |      |     ~~~~~~~         
    |      |     |
    |      |     (9) ...to here
    |   NN |       calls_free_2 (ptr);
    |      |       ~~~~~~~~~~~~~~~~~~
    |      |                    |
    |      |                    (10) passing freed pointer 'ptr' in call to 'calls_free_2' from 'test_2'
    |
    +--> 'void calls_free_2(void*)': events 11-12
           |
           |   NN | void calls_free_2 (void *ptr)
           |      |      ^~~~~~~~~~~~
           |      |      |
           |      |      (11) entry to 'calls_free_2'
           |   NN | {
           |   NN |   free (ptr);
           |      |   ~~~~~~~~~~
           |      |        |
           |      |        (12) second 'free' here; first 'free' was at (6)
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
