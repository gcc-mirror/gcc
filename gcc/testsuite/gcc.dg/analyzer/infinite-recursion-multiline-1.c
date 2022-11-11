/* Integration test of how the execution path looks for
   -Wanalyzer-infinite-recursion.  */

/* { dg-additional-options "-fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

void foo (int flag)
{
  if (flag)
    foo (flag); /* { dg-warning "infinite recursion" } */
}

/* { dg-begin-multiline-output "" }
     foo (flag);
     ^~~~~~~~~~
  'foo': events 1-4 (depth 1)
    |
    | void foo (int flag)
    |      ^~~
    |      |
    |      (1) initial entry to 'foo'
    |
    |   if (flag)
    |      ~
    |      |
    |      (2) following 'true' branch (when 'flag != 0')...
    |     foo (flag);
    |     ~~~~~~~~~~
    |     |
    |     (3) ...to here
    |     (4) calling 'foo' from 'foo'
    |
    +--> 'foo': events 5-6 (depth 2)
           |
           | void foo (int flag)
           |      ^~~
           |      |
           |      (5) recursive entry to 'foo'; previously entered at (1)
           |      (6) apparently infinite recursion
           |
   { dg-end-multiline-output "" } */
