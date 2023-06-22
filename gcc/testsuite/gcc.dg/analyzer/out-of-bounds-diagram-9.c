/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdint.h>

struct st
{
  char buf[16];
  int32_t x;
  int32_t y;
};

struct st arr[10];

int32_t struct_arr_read_x_element_before_start_far(void)
{
  return arr[-100].x; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -2384 till byte -2381 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

// TODO: show index of accessed element
// TODO: show field of accessed element
/* { dg-begin-multiline-output "" }

  ┌───────────────────────────┐
  │read of 'int32_t' (4 bytes)│
  └───────────────────────────┘
                ^
                │
                │
  ┌───────────────────────────┐            ┌─────────┬─────────┬─────────┐
  │                           │            │   [0]   │   ...   │   [9]   │
  │    before valid range     │            ├─────────┴─────────┴─────────┤
  │                           │            │'arr' (type: 'struct st[10]')│
  └───────────────────────────┘            └─────────────────────────────┘
  ├─────────────┬─────────────┤├────┬─────┤├──────────────┬──────────────┤
                │                   │                     │
     ╭──────────┴──────────╮  ╭─────┴────╮        ╭───────┴───────╮
     │under-read of 4 bytes│  │2380 bytes│        │size: 240 bytes│
     ╰─────────────────────╯  ╰──────────╯        ╰───────────────╯

   { dg-end-multiline-output "" } */
