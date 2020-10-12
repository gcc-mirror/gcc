/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-require-effective-target indirect_jumps } */

#include "test-setjmp.h"
#include <stddef.h>
#include "analyzer-decls.h"

extern int foo (int) __attribute__ ((__pure__));

static jmp_buf env;

extern int unknown_val;

static void inner (void)
{
  /* Pass value that might be 0 to longjmp. */
  longjmp (env, unknown_val);
}

void outer (void)
{
  int i;

  foo (0);

  i = SETJMP(env);

  if (i != 0)
    {
      foo (2);
      __analyzer_dump_path (); /* { dg-message "path" } */
    }
  else
    {
      foo (1);
      inner ();
    }
  foo (3);
}

/* { dg-begin-multiline-output "" }
   NN |       __analyzer_dump_path ();
      |       ^~~~~~~~~~~~~~~~~~~~~~~
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
    |   NN |   if (i != 0)
    |      |      ^
    |      |      |
    |      |      (3) following 'false' branch (when 'i == 0')...
    |......
    |   NN |       foo (1);
    |      |       ~~~~~~~
    |      |       |
    |      |       (4) ...to here
    |   NN |       inner ();
    |      |       ~~~~~~~~
    |      |       |
    |      |       (5) calling 'inner' from 'outer'
    |
    +--> 'inner': events 6-7
           |
           |   NN | static void inner (void)
           |      |             ^~~~~
           |      |             |
           |      |             (6) entry to 'inner'
           |......
           |   NN |   longjmp (env, unknown_val);
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (7) rewinding from 'longjmp' in 'inner'...
           |
    <------+
    |
  'outer': event 8
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (8) ...to 'setjmp' in 'outer' (saved at (2))
    |
  'outer': events 9-11
    |
    |   NN |   if (i != 0)
    |      |      ^
    |      |      |
    |      |      (9) following 'true' branch (when 'i != 0')...
    |   NN |     {
    |   NN |       foo (2);
    |      |       ~~~~~~~
    |      |       |
    |      |       (10) ...to here
    |   NN |       __analyzer_dump_path ();
    |      |       ~~~~~~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (11) here
    |
    { dg-end-multiline-output "" } */
