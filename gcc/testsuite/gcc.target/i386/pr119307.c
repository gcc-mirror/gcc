/* PR rtl-optimization/119307 */
/* { dg-do compile { target x32 } } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-Os -maddress-mode=long -fprofile-generate -ftrapv" } */

_Complex int x;
__int128 y;
long n;

void
foo (void)
{
  x *= *(__int128 *) __builtin_memmove (&y, &x, n);
}
