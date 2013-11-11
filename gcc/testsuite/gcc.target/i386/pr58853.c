/* { dg-do compile } */
/* { dg-options "-minline-all-stringops" } */
/* { dg-additional-options "-mtune=pentiumpro" { target { ia32 } } } */

void
my_memcpy (char *dest, const char *src, int n)
{
  __builtin_memcpy (dest, src, n);
}
