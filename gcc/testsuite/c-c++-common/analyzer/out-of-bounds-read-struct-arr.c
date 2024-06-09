#include <stdint.h>

struct st
{
  char buf[16];
  int32_t x;
  int32_t y;
};

struct st arr[10];

int32_t struct_arr_read_x_element_before_start_far(void)
{
  return arr[-100].x; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -2384 till byte -2381 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t struct_arr_read_x_element_before_start_near(void)
{
  return arr[-2].x; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -32 till byte -29 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t struct_arr_read_x_element_before_start_off_by_one(void)
{
  return arr[-1].x; /* { dg-warning "buffer under-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte -8 till byte -5 but 'arr' starts at byte 0" "final event" { target *-*-* } .-1 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-2 } */
}

int32_t struct_arr_read_x_element_at_start(void)
{
  return arr[0].x;
}

int32_t struct_arr_read_x_element_at_end(void)
{
  return arr[9].x;
}

int32_t struct_arr_read_x_element_after_end_off_by_one(void)
{
  return arr[10].x; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 256 till byte 259 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

int32_t struct_arr_read_x_element_after_end_near(void)
{
  return arr[11].x; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 280 till byte 283 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}

int32_t struct_arr_read_x_element_after_end_far(void)
{
  return arr[100].x; /* { dg-warning "buffer over-read" "warning" } */
  /* { dg-message "out-of-bounds read from byte 2416 till byte 2419 but 'arr' ends at byte 240" "final event" { target *-*-* } .-1 } */
  /* { dg-message "read of 4 bytes from after the end of 'arr'" "num bad bytes note" { target *-*-* } .-2 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } .-3 } */
}
