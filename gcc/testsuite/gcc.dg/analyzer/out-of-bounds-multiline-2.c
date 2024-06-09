/* Integration test of how the execution path looks for
   -Wanalyzer-out-of-bounds with a symbolic size.  */

/* { dg-additional-options "-fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

#include <stdint.h>
#include <stdlib.h>

void int_vla_write_element_after_end_off_by_one(int32_t x, size_t n)
{
  int32_t arr[n];

  arr[n] = x;  /* { dg-warning "stack-based buffer overflow" } */
}

/* { dg-begin-multiline-output "" }
   arr[n] = x;
   ~~~~~~~^~~
  'int_vla_write_element_after_end_off_by_one': events 1-2 (depth 1)
   int32_t arr[n];
           ^~~
           |
           (1) capacity: 'n * 4' bytes
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   arr[n] = x;
   ~~~~~~~~~~
          |
          (2) write of 4 bytes at offset 'n * 4' exceeds the buffer
   { dg-end-multiline-output "" } */
