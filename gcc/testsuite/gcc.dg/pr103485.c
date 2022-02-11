/* { dg-do compile } */

int foo_v256u128_0;
unsigned __attribute__((__vector_size__ (sizeof(unsigned) * 8))) foo_v256u8_0;

void
foo (void)
{
  foo_v256u8_0 -= (foo_v256u8_0 >> sizeof (foo_v256u8_0) - 1) + foo_v256u128_0;
}
