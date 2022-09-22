char arr[10]; /* { dg-message "capacity is 10 bytes" } */

char int_arr_read_element_before_start_far(void)
{
  return arr[-100]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read at byte -100 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

char int_arr_read_element_before_start_near(void)
{
  return arr[-2]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read at byte -2 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

char int_arr_read_element_before_start_off_by_one(void)
{
  return arr[-1]; /* { dg-warning "buffer underread" "warning" } */
  /* { dg-message "out-of-bounds read at byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

char int_arr_read_element_at_start(void)
{
  return arr[0];
}

char int_arr_read_element_at_end(void)
{
  return arr[9];
}

char int_arr_read_element_after_end_off_by_one(void)
{
  return arr[10]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read at byte 10 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): "1 bytes"
}

char int_arr_read_element_after_end_near(void)
{
  return arr[11]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read at byte 11 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): is the note correct?
  // FIXME(PR 106626): "1 bytes"
}

char int_arr_read_element_after_end_far(void)
{
  return arr[100]; /* { dg-warning "buffer overread" "warning" } */
  /* { dg-message "out-of-bounds read at byte 100 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): the note seems incorrect (size of access is 1 byte, but magnitude beyond boundary is 90)
  // FIXME(PR 106626): "1 bytes"
}
