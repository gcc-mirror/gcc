/* PR target/104462 */
/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mno-xsave" } */

typedef _Float16 __attribute__((__vector_size__ (8))) F;

F f;

void
foo (void)
{
  f *= -f;
}
