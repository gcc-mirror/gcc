/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef unsigned char __attribute__((__vector_size__ (4))) __v4qu;
typedef char __attribute__((__vector_size__ (4))) __v4qi;

__v4qu au, bu;
__v4qi as, bs;

__v4qu uu (__v4qu a, __v4qu b) { return (a > b) ? au : bu; }
__v4qu us (__v4qi a, __v4qi b) { return (a > b) ? au : bu; }
__v4qi su (__v4qu a, __v4qu b) { return (a > b) ? as : bs; }
__v4qi ss (__v4qi a, __v4qi b) { return (a > b) ? as : bs; }

/* { dg-final { scan-assembler-times "pcmpeqb" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtb" 2 } } */
