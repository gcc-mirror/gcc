/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

typedef short __attribute__((__vector_size__ (32))) V;
V g;

void
foo (void)
{
  __builtin_ia32_psrawi256 (g, 0);
}
