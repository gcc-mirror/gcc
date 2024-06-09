char arr[10]; /* { dg-message "capacity: 10 bytes" } */

char char_arr_read_element_before_start_far(void)
{
  return arr[-100]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte -100 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

char char_arr_read_element_before_start_near(void)
{
  return arr[-2]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte -2 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

char char_arr_read_element_before_start_off_by_one(void)
{
  return arr[-1]; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

char char_arr_read_element_at_start(void)
{
  return arr[0];
}

char char_arr_read_element_at_end(void)
{
  return arr[9];
}

char char_arr_read_element_after_end_off_by_one(void)
{
  return arr[10]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte 10 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 1 byte from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

char char_arr_read_element_after_end_near(void)
{
  return arr[11]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte 11 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 1 byte from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

char char_arr_read_element_after_end_far(void)
{
  return arr[100]; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read at byte 100 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 1 byte from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
