/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <string.h>
#include "analyzer-decls.h"

char *test_fixed_size_heap_2_invalid (void)
{
  char str[] = "abc";
  char *p = __builtin_malloc (strlen (str)); /* { dg-message "\\(1\\) capacity: 3 bytes" } */
  if (!p)
    return NULL;
  strcpy (p, str); /* { dg-warning "heap-based buffer overflow" } */
  return p;
}

/* { dg-begin-multiline-output "" }
  ┌──────────────────────────────────────────────────────────────────────┐
  │                           write of 4 bytes                           │
  └──────────────────────────────────────────────────────────────────────┘
                            │                                   │
                            │                                   │
                            v                                   v
  ┌───────────────────────────────────────────────────┐┌─────────────────┐
  │          buffer allocated on heap at (1)          ││after valid range│
  └───────────────────────────────────────────────────┘└─────────────────┘
  ├─────────────────────────┬─────────────────────────┤├────────┬────────┤
                            │                                   │
                   ╭────────┴────────╮                ╭─────────┴────────╮
                   │capacity: 3 bytes│                │overflow of 1 byte│
                   ╰─────────────────╯                ╰──────────────────╯
   { dg-end-multiline-output "" } */
