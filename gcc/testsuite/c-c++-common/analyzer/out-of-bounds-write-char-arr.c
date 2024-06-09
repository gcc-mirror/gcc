char arr[10]; /* { dg-message "capacity: 10 bytes" } */

void char_arr_write_element_before_start_far(char x)
{
  arr[-100] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write at byte -100 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void char_arr_write_element_before_start_near(char x)
{
  arr[-2] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write at byte -2 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void char_arr_write_element_before_start_off_by_one(char x)
{
  arr[-1] = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write at byte -1 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void char_arr_write_element_at_start(char x)
{
  arr[0] = x;
}

void char_arr_write_element_at_end(char x)
{
  arr[9] = x;
}

void char_arr_write_element_after_end_off_by_one(char x)
{
  arr[10] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 10 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 1 byte to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void char_arr_write_element_after_end_near(char x)
{
  arr[11] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 11 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 1 byte to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void char_arr_write_element_after_end_far(char x)
{
  arr[100] = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write at byte 100 but 'arr' ends at byte 10" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 1 byte to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
