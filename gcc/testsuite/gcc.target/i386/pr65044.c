/* { dg-error "-fcheck-pointer-bounds is not supported with Address Sanitizer" } */
/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fsanitize=address" } */

extern int x[];

void
foo ()
{
  x[0] = 0;
}
