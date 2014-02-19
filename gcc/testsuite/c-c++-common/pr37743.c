/* PR c/37743 */
/* This needs to be run only on targets where __UINT32_TYPE__ is defined
   to unsigned int.  */
/* { dg-do compile { target *-*-linux-gnu* } } */
/* { dg-options "-Wformat" } */

int foo (const char *, ...) __attribute__ ((format (printf, 1, 2)));

void
bar (unsigned int x)
{
  foo ("%x", __builtin_bswap32 (x));
}
