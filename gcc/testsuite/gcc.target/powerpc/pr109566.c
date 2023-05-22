/* PR target/109566 */
/* Skip this on aix, otherwise it emits the error message like "64-bit
   computation with 32-bit addressing not yet supported" on aix.  */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-do compile } */
/* { dg-options "-O2 -mpowerpc64" } */

void
foo (double x)
{
  union { double d; unsigned i; } u;
  u.d = x;
  if (u.i & 0x7ff00000)
    return;
  else
    for (;;)
      ;
}
