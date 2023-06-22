/* Test of -fanalyzer-debug-text-art.  */

/* { dg-additional-options "-fdiagnostics-text-art-charset=ascii -fanalyzer-debug-text-art" } */

#include <stdint.h>

int32_t arr[10];

void int_arr_write_element_after_end_off_by_one(int32_t x)
{
  arr[10] = x;  /* { dg-line line } */
}
/* { dg-warning "buffer overflow" "warning" { target *-*-* } line } */
/* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[9\\\]'" "valid subscript note" { target *-*-* } line } */

/* { dg-begin-multiline-output "" }

  +---------+-----------+-----------+---+--------------------------------+
  |   tc0   |    tc1    |    tc2    |tc3|              tc4               |
  +---------+-----------+-----------+---+--------------------------------+
  |bytes 0-3|bytes 4-35 |bytes 36-39|   |          bytes 40-43           |
  +---------+-----------+-----------+   +--------------------------------+
                                        +--------------------------------+
                                        |write from 'x' (type: 'int32_t')|
                                        +--------------------------------+
                                                        |
                                                        |
                                                        v
  +---------+-----------+-----------+   +--------------------------------+
  |   [0]   |    ...    |    [9]    |   |                                |
  +---------+-----------+-----------+   |       after valid range        |
  |   'arr' (type: 'int32_t[10]')   |   |                                |
  +---------------------------------+   +--------------------------------+
  |~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|   |~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|
                   |                                    |
         +---------+--------+                 +---------+---------+
         |capacity: 40 bytes|                 |overflow of 4 bytes|
         +------------------+                 +-------------------+

   { dg-end-multiline-output "" } */
