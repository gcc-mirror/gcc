/* { dg-do compile } */
/* { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } } */

unsigned long __attribute__((__vector_size__ (8 * sizeof (long)))) u;
signed long __attribute__((__vector_size__ (8 * sizeof (long)))) s;

void
foo (void)
{
  s &= u + (0, 0);
}
