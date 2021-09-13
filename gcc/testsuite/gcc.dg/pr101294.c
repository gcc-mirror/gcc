/* PR middle-end/101294 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-mavx" { target avx } } */

typedef __attribute__((__vector_size__ (sizeof (unsigned long long)))) unsigned long long U;
typedef __attribute__((__vector_size__ (4 * sizeof (unsigned long long)))) unsigned long long V;

extern U x;

void
foo (void)
{
  x = __builtin_shufflevector ((U){}, (V){}, 3);
}
