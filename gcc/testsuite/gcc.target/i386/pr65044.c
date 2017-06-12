/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fsanitize=address" } */
/* { dg-error ".-fcheck-pointer-bounds. is not supported with Address Sanitizer" "" { target *-*-* } 0 } */

extern int x[];

void
foo ()
{
  x[0] = 0;
}
