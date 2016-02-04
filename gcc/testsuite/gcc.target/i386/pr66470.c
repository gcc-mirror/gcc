/* PR target/66470 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */
/* { dg-require-effective-target tls } */

extern __thread unsigned __int128 c[10];
int d;

unsigned __int128
foo (void)
{
  return c[d];
}
