/* PR middle-end/114156 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

#if __BITINT_MAXWIDTH__ >= 128
_BitInt(128) a, b;
#else
int a, b;
#endif

void
foo (void)
{
  int u = b;
  __builtin_memmove (&a, &b, sizeof (a));
}
