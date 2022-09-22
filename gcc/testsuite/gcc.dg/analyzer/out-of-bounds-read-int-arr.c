#include <stdint.h>

int32_t arr[10]; /* { dg-message "capacity is 40 bytes" } */

int32_t int_arr_read_element_before_start_far(void)
{
  return arr[-100]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read from byte -400 till byte -397 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

int32_t int_arr_read_element_before_start_near(void)
{
  return arr[-2]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

int32_t int_arr_read_element_before_start_off_by_one(void)
{
  return arr[-1]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read from byte -4 till byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
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
  return arr[10]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read from byte 40 till byte 43 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
}

int32_t int_arr_read_element_after_end_near(void)
{
  return arr[11]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read from byte 44 till byte 47 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): is the note correct?
}

int32_t int_arr_read_element_after_end_far(void)
{
  return arr[100]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read from byte 400 till byte 403 but 'arr' ends at byte 40" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 4 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): the note seems incorrect (size of access is 4 bytes, but magnitude beyond boundary is 390-393)
}
