/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned short __attribute__((__vector_size__ (4))) __v2hu;
typedef short __attribute__((__vector_size__ (4))) __v2hi;

__v2hu au, bu;
__v2hi as, bs;

__v2hu uu (__v2hu a, __v2hu b) { return (a > b) ? au : bu; }
__v2hu us (__v2hi a, __v2hi b) { return (a > b) ? au : bu; }
__v2hi su (__v2hu a, __v2hu b) { return (a > b) ? as : bs; }
__v2hi ss (__v2hi a, __v2hi b) { return (a > b) ? as : bs; }

/* { dg-final { scan-assembler-times "pcmpeqw" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtw" 2 } } */
