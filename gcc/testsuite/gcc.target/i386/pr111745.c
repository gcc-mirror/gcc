/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -ffloat-store -O2" } */

char c;
_Float16 __attribute__((__vector_size__ (4 * sizeof (_Float16)))) f;
_Float16 __attribute__((__vector_size__ (2 * sizeof (_Float16)))) f1;

void
foo (void)
{
  f /= c;
}

void
foo1 (void)
{
  f1 /= c;
}
