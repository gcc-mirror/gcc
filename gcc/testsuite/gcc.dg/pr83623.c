/* PR middle-end/83623 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mmovbe" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

unsigned short __attribute__ ((__vector_size__ (16))) x;

void
foo (void)
{
  x = x << 8 | x >> 8;
}
