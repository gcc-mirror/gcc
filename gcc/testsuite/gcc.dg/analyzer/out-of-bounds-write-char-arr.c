char arr[10]; /* { dg-message "capacity is 10 bytes" } */

void int_arr_write_element_before_start_far(char x)
{
  arr[-100] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte -100 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

void int_arr_write_element_before_start_near(char x)
{
  arr[-2] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte -2 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

void int_arr_write_element_before_start_off_by_one(char x)
{
  arr[-1] = x; /* { dg-warning "buffer underflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
}

void int_arr_write_element_at_start(char x)
{
  arr[0] = x;
}

void int_arr_write_element_at_end(char x)
{
  arr[9] = x;
}

void int_arr_write_element_after_end_off_by_one(char x)
{
  arr[10] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 10 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): "1 bytes"
}

void int_arr_write_element_after_end_near(char x)
{
  arr[11] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 11 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): is the note correct?
  // FIXME(PR 106626): "1 bytes"
}

void int_arr_write_element_after_end_far(char x)
{
  arr[100] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 100 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write is 1 bytes past the end of 'arr'" "note" { target *-*-* } .-2 } */
  // FIXME(PR 106626): the note seems incorrect (size of access is 1 byte, but magnitude beyond boundary is 90)
  // FIXME(PR 106626): "1 bytes"
}
