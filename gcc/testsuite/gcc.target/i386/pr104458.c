/* PR target/104458 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O1 -m8bit-idiv" } */

typedef float __attribute__((__vector_size__ (8))) F;

int i;

void
foo (F f)
{
  i += i % (long long) f;
}
