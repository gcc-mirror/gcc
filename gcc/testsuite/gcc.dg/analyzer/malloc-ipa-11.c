/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include <stdlib.h>

extern int some_condition ();
extern void do_stuff (int);

void
may_call_free (void *victim)
{
  if (some_condition ())
    return;

  free (victim); /* { dg-warning "double-'free' of 'victim'" } */ 
}

void test (void *ptr)
{
  do_stuff (1);

  may_call_free (ptr);

  do_stuff (2);

  may_call_free (ptr);

  do_stuff (3);
}

/* { dg-begin-multiline-output "" }
   NN |   free (victim);
      |   ^~~~~~~~~~~~~
  'test': events 1-2
    |
    |   NN | void test (void *ptr)
    |      |      ^~~~
    |      |      |
    |      |      (1) entry to 'test'
    |......
    |   NN |   may_call_free (ptr);
    |      |   ~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (2) calling 'may_call_free' from 'test'
    |
    +--> 'may_call_free': events 3-6
           |
           |   NN | may_call_free (void *victim)
           |      | ^~~~~~~~~~~~~
           |      | |
           |      | (3) entry to 'may_call_free'
           |   NN | {
           |   NN |   if (some_condition ())
           |      |      ~
           |      |      |
           |      |      (4) following 'false' branch...
           |......
           |   NN |   free (victim);
           |      |   ~~~~~~~~~~~~~
           |      |   |
           |      |   (5) ...to here
           |      |   (6) first 'free' here
           |
    <------+
    |
  'test': events 7-8
    |
    |   NN |   may_call_free (ptr);
    |      |   ^~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (7) returning to 'test' from 'may_call_free'
    |......
    |   NN |   may_call_free (ptr);
    |      |   ~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (8) passing freed pointer 'ptr' in call to 'may_call_free' from 'test'
    |
    +--> 'may_call_free': events 9-12
           |
           |   NN | may_call_free (void *victim)
           |      | ^~~~~~~~~~~~~
           |      | |
           |      | (9) entry to 'may_call_free'
           |   NN | {
           |   NN |   if (some_condition ())
           |      |      ~
           |      |      |
           |      |      (10) following 'false' branch...
           |......
           |   NN |   free (victim);
           |      |   ~~~~~~~~~~~~~
           |      |   |
           |      |   (11) ...to here
           |      |   (12) second 'free' here; first 'free' was at (6)
           |
  { dg-end-multiline-output "" } */
