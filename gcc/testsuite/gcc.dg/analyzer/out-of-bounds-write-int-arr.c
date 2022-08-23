#include <stdint.h>

int32_t arr[10]; /* { dg-message "capacity is 40 bytes" } */

void int_arr_write_element_before_start_far(int32_t x)
{
  arr[-100] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte -400 till byte -397 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

void int_arr_write_element_before_start_near(int32_t x)
{
  arr[-2] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

void int_arr_write_element_before_start_off_by_one(int32_t x)
{
  arr[-1] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte -4 till byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
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
  /* { dg-message "write is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
}

void int_arr_write_element_after_end_near(int32_t x)
{
  arr[11] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 44 till byte 47 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): is the note correct?
}

void int_arr_write_element_after_end_far(int32_t x)
{
  arr[100] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 400 till byte 403 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): the note seems incorrect (size of access is 4 bytes, but magnitude beyond boundary is 390-393)
}
