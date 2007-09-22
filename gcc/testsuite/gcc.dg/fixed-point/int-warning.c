/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2" } */

/* N1169 Conversion from fixed-point to integer.
   Test if GCC warns when overflow or underflow.  */

signed char sc;
unsigned char uc;

void test()
{
  sc = 500k; /* { dg-warning "overflow in implicit constant conversion" } */
  sc = -500k; /* { dg-warning "overflow in implicit constant conversion" } */
  sc = 500lk; /* { dg-warning "overflow in implicit constant conversion" } */
  sc = -500lk; /* { dg-warning "overflow in implicit constant conversion" } */
  sc = 500llk; /* { dg-warning "overflow in implicit constant conversion" } */
  sc = -500llk; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = 500k; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = -500k; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = 500lk; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = -500lk; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = 500llk; /* { dg-warning "overflow in implicit constant conversion" } */
  uc = -500llk; /* { dg-warning "overflow in implicit constant conversion" } */
}
