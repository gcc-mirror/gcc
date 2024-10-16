/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Woverride-init" } */

const unsigned char a[] = {
#embed __FILE__
};
const unsigned char b[] = {
  [10] = 2, [5] = 3, [13] = 4, [17] = 5, [0] =
#embed __FILE__ suffix(,) limit (256)	/* { dg-warning "initialized field overwritten" } */
  [18] = a[18]				/* { dg-warning "initialized field overwritten" } */
};
