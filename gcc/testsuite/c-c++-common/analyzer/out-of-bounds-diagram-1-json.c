/* { dg-additional-options "-fdiagnostics-format=json-file" } */

/* The custom JSON format doesn't support text art, so this is just a simple
   smoketext.  */

#include <stdint.h>

int32_t arr[10];

void int_arr_write_element_after_end_off_by_one(int32_t x)
{
  arr[10] = x;
}
