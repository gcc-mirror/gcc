/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-require-effective-target indirect_jumps } */

#include "test-setjmp.h"
#include <stdlib.h>

extern int foo (int) __attribute__ ((__pure__));

static jmp_buf env;

static void inner (void)
{
  longjmp (env, 1); /* { dg-warning "leak of 'ptr'" } */
}

static void middle (void)
{
  void *ptr = malloc (1024);
  inner ();
  free (ptr);
}

void outer (void)
{
  int i;

  foo (0);

  i = SETJMP(env);

  if (i == 0)
    {
      foo (1);
      middle ();
    }

  foo (3);
}

/* { dg-begin-multiline-output "" }
   NN |   longjmp (env, 1);
      |   ^~~~~~~~~~~~~~~~
  'outer': event 1
    |
    |   NN | void outer (void)
    |      |      ^~~~~
    |      |      |
    |      |      (1) entry to 'outer'
    |
  'outer': event 2
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (2) 'setjmp' called here
    |
  'outer': events 3-5
    |
    |   NN |   if (i == 0)
    |      |      ^
    |      |      |
    |      |      (3) following 'true' branch (when 'i == 0')...
    |   NN |     {
    |   NN |       foo (1);
    |      |       ~~~~~~~
    |      |       |
    |      |       (4) ...to here
    |   NN |       middle ();
    |      |       ~~~~~~~~~
    |      |       |
    |      |       (5) calling 'middle' from 'outer'
    |
    +--> 'middle': events 6-8
           |
           |   NN | static void middle (void)
           |      |             ^~~~~~
           |      |             |
           |      |             (6) entry to 'middle'
           |   NN | {
           |   NN |   void *ptr = malloc (1024);
           |      |               ~~~~~~~~~~~~~
           |      |               |
           |      |               (7) allocated here
           |   NN |   inner ();
           |      |   ~~~~~~~~   
           |      |   |
           |      |   (8) calling 'inner' from 'middle'
           |
           +--> 'inner': events 9-11
                  |
                  |   NN | static void inner (void)
                  |      |             ^~~~~
                  |      |             |
                  |      |             (9) entry to 'inner'
                  |   NN | {
                  |   NN |   longjmp (env, 1);
                  |      |   ~~~~~~~~~~~~~~~~
                  |      |   |
                  |      |   (10) 'ptr' leaks here; was allocated at (7)
                  |      |   (11) rewinding from 'longjmp' in 'inner'...
                  |
    <-------------+
    |
  'outer': event 12
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (12) ...to 'setjmp' in 'outer' (saved at (2))
    |
    { dg-end-multiline-output "" } */
