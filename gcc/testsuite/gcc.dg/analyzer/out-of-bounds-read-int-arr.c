#include <stdint.h>

int32_t arr[10]; /* { dg-message "capacity: 40 bytes" } */

int32_t int_arr_read_element_before_start_far(void)
{
  return arr[-100]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -400 till byte -397 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t int_arr_read_element_before_start_near(void)
{
  return arr[-2]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t int_arr_read_element_before_start_off_by_one(void)
{
  return arr[-1]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -4 till byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t int_arr_read_element_at_start(void)
{
  return arr[0];
}

int32_t int_arr_read_element_at_end(void)
{
  return arr[9];
}

int32_t int_arr_read_element_after_end_off_by_one(void)
{
  return arr[10]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 40 till byte 43 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

int32_t int_arr_read_element_after_end_near(void)
{
  return arr[11]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 44 till byte 47 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

int32_t int_arr_read_element_after_end_far(void)
{
  return arr[100]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 400 till byte 403 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
