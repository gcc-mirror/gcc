/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-require-effective-target indirect_jumps } */

#include "test-setjmp.h"
#include <stddef.h>
#include "analyzer-decls.h"

static jmp_buf env;

static void inner (void)
{
  SETJMP (env);
}

void outer (void)
{
  int i;

  inner ();

  longjmp (env, 42); /* { dg-warning "'longjmp' called after enclosing function of 'setjmp' has returned" } */
}

/* { dg-begin-multiline-output "" }
   NN |   longjmp (env, 42);
      |   ^~~~~~~~~~~~~~~~~
  'outer': events 1-2
    |
    |   NN | void outer (void)
    |      |      ^~~~~
    |      |      |
    |      |      (1) entry to 'outer'
    |......
    |   NN |   inner ();
    |      |   ~~~~~~~~
    |      |   |
    |      |   (2) calling 'inner' from 'outer'
    |
    +--> 'inner': event 3
           |
           |   NN | static void inner (void)
           |      |             ^~~~~
           |      |             |
           |      |             (3) entry to 'inner'
           |
         'inner': event 4
           |
           |   NN |   SETJMP (env);
           |      |   ^~~~~~
           |      |   |
           |      |   (4) 'setjmp' called here
           |
         'inner': event 5
           |
           |   NN | }
           |      | ^
           |      | |
           |      | (5) stack frame is popped here, invalidating saved environment
           |
    <------+
    |
  'outer': events 6-7
    |
    |   NN |   inner ();
    |      |   ^~~~~~~~
    |      |   |
    |      |   (6) returning to 'outer' from 'inner'
    |   NN | 
    |   NN |   longjmp (env, 42);
    |      |   ~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (7) 'longjmp' called after enclosing function of 'setjmp' returned at (5)
    |
    { dg-end-multiline-output "" } */
