/* As per inlining-5.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

static inline void
inner (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'r'" } */
}

static inline void
middle (void *q)
{
  __builtin_free (q);
  inner (q);
}

void
outer (void *r)
{
  middle (r);
}

/* { dg-begin-multiline-output "" }
   __builtin_free (p);
   ^~~~~~~~~~~~~~~~~~
  'outer': events 1-2 (depth 1)
    |
    | outer (void *r)
    | ^~~~~
    | |
    | (1) entry to 'outer'
    |
    |   middle (r);
    |   ~
    |   |
    |   (2) inlined call to 'middle' from 'outer'
    |
    +--> 'middle': events 3-4 (depth 2)
           |
           |   __builtin_free (q);
           |   ^~~~~~~~~~~~~~~~~~
           |   |
           |   (3) first 'free' here
           |   inner (q);
           |   ~
           |   |
           |   (4) inlined call to 'inner' from 'middle'
           |
           +--> 'inner': event 5 (depth 3)
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
  'void outer(void*)': events 1-2 (depth 1)
    |
    | outer (void *r)
    | ^~~~~
    | |
    | (1) entry to 'outer'
    |
    |   middle (r);
    |          ~
    |          |
    |          (2) inlined call to 'middle' from 'outer'
    |
    +--> 'void middle(void*)': events 3-4 (depth 2)
           |
           |   __builtin_free (q);
           |   ~~~~~~~~~~~~~~~^~~
           |                  |
           |                  (3) first 'free' here
           |   inner (q);
           |         ~         
           |         |
           |         (4) inlined call to 'inner' from 'middle'
           |
           +--> 'void inner(void*)': event 5 (depth 3)
                  |
                  |   __builtin_free (p);
                  |   ~~~~~~~~~~~~~~~^~~
                  |                  |
                  |                  (5) second 'free' here; first 'free' was at (3)
                  |
   { dg-end-multiline-output "" { target c++ } } */
