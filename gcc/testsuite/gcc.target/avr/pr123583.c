/* { dg-do compile { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99 -w" } */

void h() {
    if(1)
      ;
  __memx const int *x = 0;
}

