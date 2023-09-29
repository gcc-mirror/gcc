/* The multiline output assumes sizeof(size_t) == 8.
   { dg-require-effective-target lp64 } */

/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

struct str {
  size_t len;
  char data[];
};

struct str *
make_str_badly (const char *src)
{
  size_t len = strlen(src);
  struct str *str = (struct str *) malloc(sizeof(str) + len); /* { dg-message "\\(1\\) capacity: 'len \\+ 8' bytes" "" { target c } } */
  /* { dg-message "\\(1\\) capacity: '\\(len \\+ 8\\)' bytes" "" { target c++ } .-1 } */
  if (!str)
    return NULL;
  str->len = len;
  memcpy(str->data, src, len);
  str->data[len] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
  return str;
}

/* { dg-begin-multiline-output "" }

                                      ┌──────────────────────────────────┐
                                      │       write of '(char) 0'        │
                                      └──────────────────────────────────┘
                                                       │
                                                       │
                                                       v
  ┌──────────────────────────────────┐┌──────────────────────────────────┐
  │ buffer allocated on heap at (1)  ││        after valid range         │
  └──────────────────────────────────┘└──────────────────────────────────┘
  ├────────────────┬─────────────────┤├────────────────┬─────────────────┤
                   │                                   │
      ╭────────────┴────────────╮            ╭─────────┴────────╮
      │capacity: 'len + 8' bytes│            │overflow of 1 byte│
      ╰─────────────────────────╯            ╰──────────────────╯

   { dg-end-multiline-output "" } */
