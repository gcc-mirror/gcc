/* We require -fdiagnostics-text-art-charset= to get any text art here
   because of the test suite using -fdiagnostics-plain-output.  */

/* { dg-additional-options "-fdiagnostics-format=sarif-file -fdiagnostics-text-art-charset=ascii" } */

#include <stdint.h>

int32_t arr[10];

void int_arr_write_element_after_end_off_by_one(int32_t x)
{
  arr[10] = x;
}

/* Verify that some JSON was written to a file with the expected name.

   { dg-final { verify-sarif-file } }

   Expect the "alt-text" to be captured.
     { dg-final { scan-sarif-file "\"text\": \"Diagram visualizing the predicted out-of-bounds access\"," } }

   Expect the diagram to have 4 leading spaces (to indicate a code block),
   and that at least part of the diagram was written out.
     { dg-final { scan-sarif-file "\"markdown\": \"    .*capacity: 40 bytes.*\"" } } */
