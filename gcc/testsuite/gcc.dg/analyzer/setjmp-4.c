/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include "test-setjmp.h"
#include "analyzer-decls.h"

extern int foo (int) __attribute__ ((__pure__));
static jmp_buf buf;

void inner (int x)
{
  foo (x);
  longjmp (buf, 1);
  foo (x);
}

void outer (int y)
{
  foo (y);
  inner (y);
  foo (y);
}

int main (void)
{   
  if (!SETJMP(buf))
    outer (42);
  else
    __analyzer_dump_path (); /* { dg-message "path" } */
  return 0;
}

/* { dg-begin-multiline-output "" }
   NN |     __analyzer_dump_path ();
      |     ^~~~~~~~~~~~~~~~~~~~~~~
  'main': event 1
    |
    |   NN | int main (void)
    |      |     ^~~~
    |      |     |
    |      |     (1) entry to 'main'
    |
  'main': event 2
    |
    |   NN |   if (!SETJMP(buf))
    |      |        ^~~~~~
    |      |        |
    |      |        (2) 'setjmp' called here
    |
  'main': events 3-5
    |
    |   NN |   if (!SETJMP(buf))
    |      |      ^
    |      |      |
    |      |      (3) following 'true' branch...
    |   NN |     outer (42);
    |      |     ~~~~~~~~~~
    |      |     |
    |      |     (4) ...to here
    |      |     (5) calling 'outer' from 'main'
    |
    +--> 'outer': events 6-7
           |
           |   NN | void outer (int y)
           |      |      ^~~~~
           |      |      |
           |      |      (6) entry to 'outer'
           |......
           |   NN |   inner (y);
           |      |   ~~~~~~~~~
           |      |   |
           |      |   (7) calling 'inner' from 'outer'
           |
           +--> 'inner': events 8-9
                  |
                  |   NN | void inner (int x)
                  |      |      ^~~~~
                  |      |      |
                  |      |      (8) entry to 'inner'
                  |......
                  |   NN |   longjmp (buf, 1);
                  |      |   ~~~~~~~~~~~~~~~~
                  |      |   |
                  |      |   (9) rewinding from 'longjmp' in 'inner'...
                  |
    <-------------+
    |
  'main': event 10
    |
    |   NN |   if (!SETJMP(buf))
    |      |        ^~~~~~
    |      |        |
    |      |        (10) ...to 'setjmp' in 'main' (saved at (2))
    |
  'main': events 11-13
    |
    |   NN |   if (!SETJMP(buf))
    |      |      ^
    |      |      |
    |      |      (11) following 'false' branch...
    |......
    |   NN |     __analyzer_dump_path ();
    |      |     ~~~~~~~~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (12) ...to here
    |      |     (13) here
    |
    { dg-end-multiline-output "" } */

