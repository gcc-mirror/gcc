/* As per inlining-2.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

static void __analyzer_foo (void *p)
{
  __builtin_free (p);

  __builtin_free (p); /* { dg-warning "double-'free' of 'q'" "warning" } */
}

void bar (void *q)
{
  __analyzer_foo (q);
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
    |   __analyzer_foo (q);
    |   ~   
    |   |
    |   (2) inlined call to '__analyzer_foo' from 'bar'
    |
    +--> '__analyzer_foo': events 3-4 (depth 2)
           |
           |   __builtin_free (p);
           |   ^~~~~~~~~~~~~~~~~~
           |   |
           |   (3) first 'free' here
           |
           |   __builtin_free (p);
           |   ~~~~~~~~~~~~~~~~~~
           |   |
           |   (4) second 'free' here; first 'free' was at (3)
           |
   { dg-end-multiline-output "" } */
