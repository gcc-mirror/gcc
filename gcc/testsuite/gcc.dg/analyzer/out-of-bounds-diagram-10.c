/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdint.h>
#include <stdlib.h>

int32_t int_vla_write_element_symbolic_before_start (int32_t x, size_t n)
{
  int32_t arr[n]; /* { dg-message "\\(1\\) capacity: 'n \\* 4' bytes" } */
  arr[-2] = 42;  /* { dg-warning "stack-based buffer underwrite" } */
}

/* { dg-begin-multiline-output "" }

  ┌───────────────────┐
  │write of '(int) 42'│
  └───────────────────┘
            │
            │
            v
  ┌───────────────────┐                 ┌────────────────────────────────┐
  │before valid range │                 │buffer allocated on stack at (1)│
  └───────────────────┘                 └────────────────────────────────┘
  ├─────────┬─────────┤├───────┬───────┤├───────────────┬────────────────┤
            │                  │                        │
  ╭─────────┴───────────╮  ╭───┴───╮        ╭───────────┴───────────╮
  │underwrite of 4 bytes│  │4 bytes│        │capacity: 'n * 4' bytes│
  ╰─────────────────────╯  ╰───────╯        ╰───────────────────────╯

   { dg-end-multiline-output "" } */
