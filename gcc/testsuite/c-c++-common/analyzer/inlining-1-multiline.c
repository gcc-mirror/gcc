/* As per inlining-1.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

void foo (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'q'" "warning" } */
}

void bar (void *q)
{
  foo (q);
  foo (q);
}

/* { dg-begin-multiline-output "" }
   __builtin_free (p);
   ^~~~~~~~~~~~~~~~~~
  'bar': events 1-2 (depth 1)
    |
    | void bar (void *q)
    |      ^~~
    |      |
    |      (1) entry to 'bar'
    |
    |   foo (q);
    |   ~   
    |   |
    |   (2) inlined call to 'foo' from 'bar'
    |
    +--> 'foo': event 3 (depth 2)
           |
           |   __builtin_free (p);
           |   ^~~~~~~~~~~~~~~~~~
           |   |
           |   (3) first 'free' here
           |
    <------+
    |
  'bar': event 4 (depth 1)
    |
    |   foo (q);
    |   ^
    |   |
    |   (4) inlined call to 'foo' from 'bar'
    |
    +--> 'foo': event 5 (depth 2)
           |
           |   __builtin_free (p);
           |   ^~~~~~~~~~~~~~~~~~
           |   |
           |   (5) second 'free' here; first 'free' was at (3)
           |
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   __builtin_free (p);
   ~~~~~~~~~~~~~~~^~~
  'void bar(void*)': events 1-2 (depth 1)
    |
    | void bar (void *q)
    |      ^~~
    |      |
    |      (1) entry to 'bar'
    |
    |   foo (q);
    |       ~
    |       |
    |       (2) inlined call to 'foo' from 'bar'
    |
    +--> 'void foo(void*)': event 3 (depth 2)
           |
           |   __builtin_free (p);
           |   ~~~~~~~~~~~~~~~^~~
           |                  |
           |                  (3) first 'free' here
           |
    <------+
    |
  'void bar(void*)': event 4 (depth 1)
    |
    |   foo (q);
    |       ^
    |       |
    |       (4) inlined call to 'foo' from 'bar'
    |
    +--> 'void foo(void*)': event 5 (depth 2)
           |
           |   __builtin_free (p);
           |   ~~~~~~~~~~~~~~~^~~
           |                  |
           |                  (5) second 'free' here; first 'free' was at (3)
           |
   { dg-end-multiline-output "" { target c++ } } */
