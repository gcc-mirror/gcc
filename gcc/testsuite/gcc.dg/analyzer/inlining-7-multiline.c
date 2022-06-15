/* As per inlining-7.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

static inline void
depth_6 (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'p1'" "warning" } */
}

static inline void
depth_5 (void *p5)
{
  depth_6 (p5);
}

static inline void
depth_4 (void *p4)
{
  depth_5 (p4);
}

static inline void
depth_3 (void *p3)
{
  depth_4 (p3);
  depth_4 (p3);
}

static inline void
depth_2 (void *p2)
{
  depth_3 (p2);
}

void
depth_1 (void *p1)
{
  depth_2 (p1);
}

/* We want the reconstructed call/return hierarchy to show
   that two calls happen at depth_3, without popping the stack
   back any further.  */

/* { dg-begin-multiline-output "" }
   __builtin_free (p);
   ^~~~~~~~~~~~~~~~~~
  'depth_1': events 1-2 (depth 1)
    |
    | depth_1 (void *p1)
    | ^~~~~~~
    | |
    | (1) entry to 'depth_1'
    |
    |   depth_2 (p1);
    |   ~
    |   |
    |   (2) inlined call to 'depth_2' from 'depth_1'
    |
    +--> 'depth_2': event 3 (depth 2)
           |
           |   depth_3 (p2);
           |   ^
           |   |
           |   (3) inlined call to 'depth_3' from 'depth_2'
           |
           +--> 'depth_3': event 4 (depth 3)
                  |
                  |   depth_4 (p3);
                  |   ^
                  |   |
                  |   (4) inlined call to 'depth_4' from 'depth_3'
                  |
                  +--> 'depth_4': event 5 (depth 4)
                         |
                         |   depth_5 (p4);
                         |   ^
                         |   |
                         |   (5) inlined call to 'depth_5' from 'depth_4'
                         |
                         +--> 'depth_5': event 6 (depth 5)
                                |
                                |   depth_6 (p5);
                                |   ^
                                |   |
                                |   (6) inlined call to 'depth_6' from 'depth_5'
                                |
                                +--> 'depth_6': event 7 (depth 6)
                                       |
                                       |   __builtin_free (p);
                                       |   ^~~~~~~~~~~~~~~~~~
                                       |   |
                                       |   (7) first 'free' here
                                       |
                  <--------------------+
                  |
                'depth_3': event 8 (depth 3)
                  |
                  |   depth_4 (p3);
                  |   ^
                  |   |
                  |   (8) inlined call to 'depth_4' from 'depth_3'
                  |
                  +--> 'depth_4': event 9 (depth 4)
                         |
                         |   depth_5 (p4);
                         |   ^
                         |   |
                         |   (9) inlined call to 'depth_5' from 'depth_4'
                         |
                         +--> 'depth_5': event 10 (depth 5)
                                |
                                |   depth_6 (p5);
                                |   ^
                                |   |
                                |   (10) inlined call to 'depth_6' from 'depth_5'
                                |
                                +--> 'depth_6': event 11 (depth 6)
                                       |
                                       |   __builtin_free (p);
                                       |   ^~~~~~~~~~~~~~~~~~
                                       |   |
                                       |   (11) second 'free' here; first 'free' was at (7)
                                       |
   { dg-end-multiline-output "" } */
