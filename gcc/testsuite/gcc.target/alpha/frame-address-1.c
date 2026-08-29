/* An Alpha frame carries no link to its caller, so __builtin_frame_address
   must decline a nonzero count rather than return the saved return address,
   which is what offset 0 of a frame actually holds.  */

/* { dg-do compile } */
/* { dg-skip-if "builtins are not expanded at compile time" { *-*-* } { "-flto" } { "" } } */

void *
fa0 (void)
{
  return __builtin_frame_address (0);
}

void *
fa1 (void)
{
  return __builtin_frame_address (1); /* { dg-warning "unsupported argument to '__builtin_frame_address'" } */
}
