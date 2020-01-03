/* PR target/93110 */
/* { dg-do compile } */
/* { dg-additional-options "-mtune=core2 -mno-stv" { target { i?86-*-* x86_64-*-* } } } */

long long
foo (long long a)
{
  return a > 0 ? a : -a;
}
