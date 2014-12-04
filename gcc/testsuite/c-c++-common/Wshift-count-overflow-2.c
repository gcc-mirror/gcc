/* { dg-do compile } */
/* { dg-options "-Wno-shift-count-overflow" } */

void foo()
{
  unsigned i1 = 1U << (sizeof(unsigned) * __CHAR_BIT__);
  unsigned i2 = 1U >> (sizeof(unsigned) * __CHAR_BIT__);
}
