/* { dg-do compile } */
/* { dg-options "-Wshift-count-overflow" } */

void foo()
{
  unsigned i1 = 1U << (sizeof(unsigned) * __CHAR_BIT__); /* { dg-warning "20:left shift count >= width of type" } */
  unsigned i2 = 1U >> (sizeof(unsigned) * __CHAR_BIT__); /* { dg-warning "20:right shift count >= width of type" } */
}
