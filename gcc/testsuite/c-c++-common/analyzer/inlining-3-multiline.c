/* As per inlining-3.c, but testing how the ASCII art version of
   the path looks.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"
typedef __SIZE_TYPE__ size_t;

struct input_file_st 
{
  char inpname[1];
};

typedef struct input_file_st input_file;

static inline const char*
get_input_file_name (const input_file *inpf)
{
  if (inpf)
    return inpf->inpname;
  return NULL;
}

size_t
test (const input_file *inpf)
{
  const char *f = get_input_file_name (inpf);
  return __builtin_strlen (f); /* { dg-warning "use of NULL" "warning" } */
}

/* { dg-begin-multiline-output "" }
   return __builtin_strlen (f);
          ^~~~~~~~~~~~~~~~~~~~
  'test': events 1-2 (depth 1)
    |
    | test (const input_file *inpf)
    | ^~~~
    | |
    | (1) entry to 'test'
    |
    |   const char *f = get_input_file_name (inpf);
    |                   ~
    |                   |
    |                   (2) inlined call to 'get_input_file_name' from 'test'
    |
    +--> 'get_input_file_name': event 3 (depth 2)
           |
           |   if (inpf)
           |      ^
           |      |
           |      (3) following 'false' branch (when 'inpf' is NULL)...
           |
    <------+
    |
  'test': events 4-5 (depth 1)
    |
    |   return __builtin_strlen (f);
    |          ^~~~~~~~~~~~~~~~~~~~
    |          |
    |          (4) ...to here
    |          (5) argument 1 ('<unknown>') NULL where non-null expected
    |
   { dg-end-multiline-output "" { target c } } */


/* { dg-begin-multiline-output "" }
   return __builtin_strlen (f);
          ~~~~~~~~~~~~~~~~~^~~
  'size_t test(const input_file*)': events 1-2 (depth 1)
    |
    | test (const input_file *inpf)
    | ^~~~
    | |
    | (1) entry to 'test'
    |
    |   const char *f = get_input_file_name (inpf);
    |                                       ~
    |                                       |
    |                                       (2) inlined call to 'get_input_file_name' from 'test'
    |
    +--> 'const char* get_input_file_name(const input_file*)': event 3 (depth 2)
           |
           |   if (inpf)
           |   ^~
           |   |
           |   (3) following 'false' branch (when 'inpf' is NULL)...
           |
    <------+
    |
  'size_t test(const input_file*)': events 4-5 (depth 1)
    |
    |   return __builtin_strlen (f);
    |          ~~~~~~~~~~~~~~~~~^~~
    |                           |
    |                           (4) ...to here
    |                           (5) argument 1 ('<unknown>') NULL where non-null expected
    |
   { dg-end-multiline-output "" { target c++ } } */