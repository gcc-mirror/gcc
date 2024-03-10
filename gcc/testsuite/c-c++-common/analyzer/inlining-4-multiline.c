/* As per inlining-4.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"


static inline const char*
inner (int flag)
{
  if (flag)
    return NULL;
  return "foo";
}

static inline const char*
middle (int flag)
{
  return inner (flag);
}

char
outer (int flag)
{
  return *middle (flag); /* { dg-warning "dereference of NULL" "warning" } */
}

/* { dg-begin-multiline-output "" }
   return *middle (flag);
          ^~~~~~~~~~~~~~
  'outer': events 1-2 (depth 1)
    |
    | outer (int flag)
    | ^~~~~
    | |
    | (1) entry to 'outer'
    |
    |   return *middle (flag);
    |           ~
    |           |
    |           (2) inlined call to 'middle' from 'outer'
    |
    +--> 'middle': event 3 (depth 2)
           |
           |   return inner (flag);
           |          ^
           |          |
           |          (3) inlined call to 'inner' from 'middle'
           |
           +--> 'inner': event 4 (depth 3)
                  |
                  |   if (flag)
                  |      ^
                  |      |
                  |      (4) following 'true' branch (when 'flag != 0')...
                  |
    <-------------+
    |
  'outer': event 5 (depth 1)
    |
    |cc1:
    | (5): ...to here
    |
  'outer': event 6 (depth 1)
    |
    |   return *middle (flag);
    |          ^~~~~~~~~~~~~~
    |          |
    |          (6) dereference of NULL '<unknown>'
    |
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   return *middle (flag);
                       ^
  'char outer(int)': events 1-2 (depth 1)
    |
    | outer (int flag)
    | ^~~~~
    | |
    | (1) entry to 'outer'
    |
    |   return *middle (flag);
    |                  ~
    |                  |
    |                  (2) inlined call to 'middle' from 'outer'
    |
    +--> 'const char* middle(int)': event 3 (depth 2)
           |
           |   return inner (flag);
           |                ^
           |                |
           |                (3) inlined call to 'inner' from 'middle'
           |
           +--> 'const char* inner(int)': event 4 (depth 3)
                  |
                  |   if (flag)
                  |   ^~
                  |   |
                  |   (4) following 'true' branch (when 'flag != 0')...
                  |
    <-------------+
    |
  'char outer(int)': event 5 (depth 1)
    |
    |cc1plus:
    | (5): ...to here
    |
  'char outer(int)': event 6 (depth 1)
    |
    |   return *middle (flag);
    |                       ^
    |                       |
    |                       (6) dereference of NULL '<unknown>'
    |
   { dg-end-multiline-output "" { target c++ } } */
