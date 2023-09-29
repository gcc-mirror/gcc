/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-require-effective-target indirect_jumps } */

#include "../../gcc.dg/analyzer/test-setjmp.h"
#include <stddef.h>
#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void foo (int);

void test_1 (void)
{
  SETJMP (NULL);
}

void test_2 (void)
{
  jmp_buf env;
  int i;

  foo (0);

  i = SETJMP(env);

  foo (1);

  if (i != 0)
    {
      foo (2);
      __analyzer_dump_path (); /* { dg-message "path" } */
    }
  else
    longjmp (env, 1);

  foo (3);
}

/* { dg-begin-multiline-output "" }
   NN |       __analyzer_dump_path ();
      |       ^~~~~~~~~~~~~~~~~~~~~~~
  'test_2': event 1
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (1) 'setjmp' called here
    |
  'test_2': events 2-4
    |
    |   NN |   if (i != 0)
    |      |      ^
    |      |      |
    |      |      (2) following 'false' branch (when 'i == 0')...
    |......
    |   NN |     longjmp (env, 1);
    |      |     ~~~~~~~~~~~~~~~~
    |      |     |
    |      |     (3) ...to here
    |      |     (4) rewinding within 'test_2' from 'longjmp'...
    |
  'test_2': event 5
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (5) ...to 'setjmp' (saved at (1))
    |
  'test_2': events 6-8
    |
    |   NN |   if (i != 0)
    |      |      ^
    |      |      |
    |      |      (6) following 'true' branch (when 'i != 0')...
    |   NN |     {
    |   NN |       foo (2);
    |      |       ~~~~~~~
    |      |       |
    |      |       (7) ...to here
    |   NN |       __analyzer_dump_path ();
    |      |       ~~~~~~~~~~~~~~~~~~~~~~~
    |      |       |
    |      |       (8) here
    |
    { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |       __analyzer_dump_path ();
      |       ~~~~~~~~~~~~~~~~~~~~~^~
  'void test_2()': event 1
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (1) 'setjmp' called here
    |
  'void test_2()': events 2-4
    |
    |   NN |   if (i != 0)
    |      |   ^~
    |      |   |
    |      |   (2) following 'false' branch (when 'i == 0')...
    |......
    |   NN |     longjmp (env, 1);
    |      |     ~~~~~~~~~~~~~~~~
    |      |             |
    |      |             (3) ...to here
    |      |             (4) rewinding within 'test_2' from 'longjmp'...
    |
  'void test_2()': event 5
    |
    |   NN |   i = SETJMP(env);
    |      |       ^~~~~~
    |      |       |
    |      |       (5) ...to 'setjmp' (saved at (1))
    |
  'void test_2()': events 6-8
    |
    |   NN |   if (i != 0)
    |      |   ^~
    |      |   |
    |      |   (6) following 'true' branch (when 'i != 0')...
    |   NN |     {
    |   NN |       foo (2);
    |      |       ~~~~~~~
    |      |           |
    |      |           (7) ...to here
    |   NN |       __analyzer_dump_path ();
    |      |       ~~~~~~~~~~~~~~~~~~~~~~~
    |      |                            |
    |      |                            (8) here
    |
    { dg-end-multiline-output "" { target c++ } } */

void test_3 (void)
{
  longjmp (NULL, 0);
}

void test_4 (void)
{
  longjmp (NULL, 1);
}

void test_5 (void)
{
  jmp_buf env;
  longjmp (env, 1);
}
