/* { dg-do compile } */
/* { dg-options "-O0 -fno-omit-frame-pointer -fvar-tracking-assignments" } */

register long *B asm ("ebp");

long y = 20;

void
bar (void) /* { dg-error "frame pointer required, but reserved" } */
{
  B = &y;
} /* { dg-error "bp cannot be used in asm here" } */
