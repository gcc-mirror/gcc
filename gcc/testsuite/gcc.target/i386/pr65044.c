/* { dg-error "-fcheck-pointer-bounds is not supported with Address Sanitizer" } */
/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fsanitize=address" } */

extern int x[];

void
foo ()
{
  x[0] = 0;
}
