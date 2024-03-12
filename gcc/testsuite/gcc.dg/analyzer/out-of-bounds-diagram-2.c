/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdint.h>
#include <stdlib.h>

void int_vla_write_element_after_end_off_by_one(int32_t x, size_t n)
{
  int32_t arr[n]; /* { dg-message "\\(1\\) capacity: 'n \\* 4' bytes" } */

  arr[n] = x;  /* { dg-warning "stack-based buffer overflow" } */
}

/* { dg-begin-multiline-output "" }

                                      ┌──────────────────────────────────┐
                                      │ write from 'x' (type: 'int32_t') │
                                      └──────────────────────────────────┘
                                                       │
                                                       │
                                                       v
  ┌──────────────────────────────────┐┌──────────────────────────────────┐
  │ buffer allocated on stack at (1) ││        after valid range         │
  └──────────────────────────────────┘└──────────────────────────────────┘
  ├────────────────┬─────────────────┤├────────────────┬─────────────────┤
                   │                                   │
       ╭───────────┴───────────╮             ╭─────────┴─────────╮
       │capacity: 'n * 4' bytes│             │overflow of 4 bytes│
       ╰───────────────────────╯             ╰───────────────────╯

   { dg-end-multiline-output "" } */
