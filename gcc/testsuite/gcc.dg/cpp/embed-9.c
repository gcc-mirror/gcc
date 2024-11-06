/* { dg-do compile } */
/* { dg-options "-std=c23 -Woverride-init" } */

unsigned char a[] = {
#embed __FILE__
};
unsigned char b[] = {
  [26] =
#embed __FILE__
};
unsigned char c[] = {
#embed __FILE__ suffix (,)
  [sizeof (a) / 4] = 0,		/* { dg-warning "initialized field overwritten" } */
  [sizeof (a) / 2] = 1,		/* { dg-warning "initialized field overwritten" } */
  [1] = 2,			/* { dg-warning "initialized field overwritten" } */
  [sizeof (a) - 2] = 3		/* { dg-warning "initialized field overwritten" } */
};
unsigned char d[] = {
  [1] = 4,
  [26] = 5,
  [sizeof (a) / 4] = 6,
  [sizeof (a) / 2] = 7,
  [sizeof (a) - 2] = 8,
#embed __FILE__ prefix ([0] = )	/* { dg-warning "initialized field overwritten" } */
};
unsigned char e[] = {
#embed __FILE__ suffix (,)
  [2] = 9,			/* { dg-warning "initialized field overwritten" } */
  [sizeof (a) - 3] = 10		/* { dg-warning "initialized field overwritten" } */
};
unsigned char f[] = {
  [23] = 11,
  [sizeof (a) / 4 - 1] = 12,
#embed __FILE__ limit (128) prefix ([sizeof (a) / 4 - 1] = ) suffix (,)		/* { dg-warning "initialized field overwritten" } */
#embed __FILE__ limit (130) prefix ([sizeof (a) / 4 - 2] = ) suffix (,)		/* { dg-warning "initialized field overwritten" } */
#embed __FILE__ prefix ([sizeof (a) / 4 + 10] = ) suffix (,)			/* { dg-warning "initialized field overwritten" } */
#embed __FILE__ limit (128) prefix ([sizeof (a) + sizeof (a) / 4 - 30] = ) suffix (,) /* { dg-warning "initialized field overwritten" } */
#embed __FILE__ limit (128) prefix ([sizeof (a) / 4 + 96] = ) suffix (,)	/* { dg-warning "initialized field overwritten" } */
};
const unsigned char g[] = {
#embed __FILE__ limit (128) prefix (  [10] = 2, [5] = 3, [13] = 4, [17] = 5, [0] = )	/* { dg-warning "initialized field overwritten" } */
};
