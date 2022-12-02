#include <stdint.h>

int32_t arr[10]; /* { dg-message "capacity: 40 bytes" } */

void int_arr_write_element_before_start_far(int32_t x)
{
  arr[-100] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -400 till byte -397 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void int_arr_write_element_before_start_near(int32_t x)
{
  arr[-2] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void int_arr_write_element_before_start_off_by_one(int32_t x)
{
  arr[-1] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -4 till byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void int_arr_write_element_at_start(int32_t x)
{
  arr[0] = x;
}

void int_arr_write_element_at_end(int32_t x)
{
  arr[9] = x;
}

void int_arr_write_element_after_end_off_by_one(int32_t x)
{
  arr[10] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 40 till byte 43 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void int_arr_write_element_after_end_near(int32_t x)
{
  arr[11] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 44 till byte 47 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void int_arr_write_element_after_end_far(int32_t x)
{
  arr[100] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 400 till byte 403 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
