/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } } */

#include <stdlib.h>
#include <stdint.h>

/* Gap of 4, then an overflow of 4.  */

void test2 (size_t size)
{
  int32_t *buf = (int32_t *) __builtin_malloc (size * sizeof(int32_t)); /* { dg-message "\\(1\\) capacity: 'size \\* 4' bytes" "" { target c } } */
  /* { dg-message "\\(1\\) capacity: '\\(size \\* 4\\)' bytes" "" { target c++ } .-1 } */
  if (!buf) return;

  buf[size + 1] = 42; /* { dg-warning "heap-based buffer overflow" } */
  __builtin_free (buf);
}

/* { dg-begin-multiline-output "" }

                                                 ┌───────────────────────┐
                                                 │write of '(int32_t) 42'│
                                                 └───────────────────────┘
                                                             │
                                                             │
                                                             v
  ┌───────────────────────────────┐              ┌───────────────────────┐
  │buffer allocated on heap at (1)│              │   after valid range   │
  └───────────────────────────────┘              └───────────────────────┘
  ├───────────────┬───────────────┤├─────┬──────┤├───────────┬───────────┤
                  │                      │                   │
    ╭─────────────┴────────────╮     ╭───┴───╮     ╭─────────┴─────────╮
    │capacity: 'size * 4' bytes│     │4 bytes│     │overflow of 4 bytes│
    ╰──────────────────────────╯     ╰───────╯     ╰───────────────────╯

   { dg-end-multiline-output "" } */
