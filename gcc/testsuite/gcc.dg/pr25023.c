/* PR debug/25023 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

extern unsigned char v;

float
foo (void)
{
  return v;
}
