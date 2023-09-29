/* As per inlining-6.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

static inline void
inner (void *p)
{
  __builtin_free (p);
}

static inline void
middle (void *q)
{
  inner (q);
  __builtin_free (q); /* { dg-warning "double-'free' of 'r'" } */
}

void
outer (void *r)
{
  middle (r);
}

/* { dg-begin-multiline-output "" }
   __builtin_free (q);
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
    +--> 'middle': event 3 (depth 2)
           |
           |   inner (q);
           |   ^
           |   |
           |   (3) inlined call to 'inner' from 'middle'
           |
           +--> 'inner': event 4 (depth 3)
                  |
                  |   __builtin_free (p);
                  |   ^~~~~~~~~~~~~~~~~~
                  |   |
                  |   (4) first 'free' here
                  |
           <------+
           |
         'middle': event 5 (depth 2)
           |
           |   __builtin_free (q);
           |   ^~~~~~~~~~~~~~~~~~
           |   |
           |   (5) second 'free' here; first 'free' was at (4)
           |
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   __builtin_free (q);
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
    +--> 'void middle(void*)': event 3 (depth 2)
           |
           |   inner (q);
           |         ^
           |         |
           |         (3) inlined call to 'inner' from 'middle'
           |
           +--> 'void inner(void*)': event 4 (depth 3)
                  |
                  |   __builtin_free (p);
                  |   ~~~~~~~~~~~~~~~^~~
                  |                  |
                  |                  (4) first 'free' here
                  |
           <------+
           |
         'void middle(void*)': event 5 (depth 2)
           |
           |   __builtin_free (q);
           |   ~~~~~~~~~~~~~~~^~~
           |                  |
           |                  (5) second 'free' here; first 'free' was at (4)
           |
   { dg-end-multiline-output "" { target c++ } } */
