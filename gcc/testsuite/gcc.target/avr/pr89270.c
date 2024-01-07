/* { dg-do compile { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

void test()
{
  extern const unsigned char __memx __data_load_end;
  __uint24 top=(__uint24)&__data_load_end;
}
