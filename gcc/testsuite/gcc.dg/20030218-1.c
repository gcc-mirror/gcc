/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mcpu=8540" } */

/* Test vectors that can interconvert without a cast.  */

int vint __attribute__((mode(V2SI)));
int vshort __attribute__((mode(V4HI)));
int vfloat __attribute__((mode(V2SF)));

int
main (void)
{
  vint = vfloat;
  vshort = vint;
  vfloat = vshort; /* { dg-error "incompatible types in assignment" } */
}
