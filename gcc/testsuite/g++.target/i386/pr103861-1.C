/* PR target/103861 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4.1" } */

typedef unsigned char __attribute__((__vector_size__ (2))) __v2qu;
typedef char __attribute__((__vector_size__ (2))) __v2qi;

__v2qu au, bu;
__v2qi as, bs;

__v2qu uu (__v2qu a, __v2qu b) { return (a > b) ? au : bu; }
__v2qu us (__v2qi a, __v2qi b) { return (a > b) ? au : bu; }
__v2qi su (__v2qu a, __v2qu b) { return (a > b) ? as : bs; }
__v2qi ss (__v2qi a, __v2qi b) { return (a > b) ? as : bs; }

/* { dg-final { scan-assembler-times "pcmpeqb" 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "pcmpgtb" 2 } } */
