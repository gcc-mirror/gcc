/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Verify that -Wframe-address is included in -Wall.  */

void* test_builtin_address (unsigned i)
{
  void* const ba[] = {
    __builtin_frame_address (4), /* { dg-warning "builtin_frame_address" } */
    __builtin_return_address (4)  /* { dg-warning "builtin_return_address" } */
  };

  return ba [i];
}
