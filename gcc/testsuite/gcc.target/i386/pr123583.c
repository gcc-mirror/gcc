/* { dg-do compile } */
/* { dg-additional-options "-std=gnu99 -w" } */

void h() {
    if(1)
      ;
  __seg_gs const int *x = 0;
}
