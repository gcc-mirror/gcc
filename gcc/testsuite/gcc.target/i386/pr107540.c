/* { dg-do compile } */
/* { dg-options "-flive-range-shrinkage -mavx" } */

typedef double __attribute__((__vector_size__ (32))) V;

V v;

void
foo (void)
{
  v = __builtin_ia32_movddup256 (v);
}
