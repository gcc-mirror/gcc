#include <stdint.h>

struct st
{
  char buf[16];
  int32_t x;
  int32_t y;
};

struct st arr[10];

void struct_arr_write_x_element_before_start_far(int32_t x)
{
  arr[-100].x = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -2384 till byte -2381 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void struct_arr_write_x_element_before_start_near(int32_t x)
{
  arr[-2].x = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -32 till byte -29 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void struct_arr_write_x_element_before_start_off_by_one(int32_t x)
{
  arr[-1].x = x; /* { dg-warning "buffer underwrite" "warning" } */
  /* { dg-message "out-of-bounds write from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

void struct_arr_write_x_element_at_start(int32_t x)
{
  arr[0].x = x;
}

void struct_arr_write_x_element_at_end(int32_t x)
{
  arr[9].x = x;
}

void struct_arr_write_x_element_after_end_off_by_one(int32_t x)
{
  arr[10].x = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 256 till byte 259 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void struct_arr_write_x_element_after_end_near(int32_t x)
{
  arr[11].x = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 280 till byte 283 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

void struct_arr_write_x_element_after_end_far(int32_t x)
{
  arr[100].x = x; /* { dg-warning "buffer overflow" "warning" } */
  /* { dg-message "out-of-bounds write from byte 2416 till byte 2419 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
