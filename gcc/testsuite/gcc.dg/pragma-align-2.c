/* { dg-do run { target *-*-solaris2.* } } */

void abort (void);

#pragma align 1(x1)
#pragma align 2(x2)
#pragma align 4(x4)
#pragma align 8(x8,y8,z8)
#pragma align 16(x16)
#pragma align 32(x32)
#pragma align 64(x64)
#pragma align 128(x128)

#define MACRO 128
#define MACRO2(A) A

#pragma align MACRO(y128)
#pragma align MACRO2(MACRO) (z128)

#pragma align 8(not_defined)

#pragma align 9(odd_align)	/* { dg-warning "invalid alignment" } */
#pragma align 256(high_align)	/* { dg-warning "invalid alignment" } */
#pragma align -1(neg_align)	/* { dg-warning "malformed" } */
#pragma align bad_align		/* { dg-warning "malformed" } */
#pragma align 1(bad_align	/* { dg-warning "malformed" } */

int x, x1, x2, x4, x8, y8, z8, x16, x32, x64, x128, y128, z128;

#pragma align 16(x)		/* { dg-warning "must appear before" } */

int
main ()
{
  if (__alignof__ (x4) < 4)
    abort ();

  if (__alignof__ (x8) < 8)
    abort ();

  if (__alignof__ (y8) < 8)
    abort ();

  if (__alignof__ (z8) < 8)
    abort ();

  if (__alignof__ (x16) < 16)
    abort ();

  if (__alignof__ (x32) < 32)
    abort ();

  if (__alignof__ (x64) < 64)
    abort ();

  if (__alignof__ (x128) < 128)
    abort ();

  if (__alignof__ (y128) < 128)
    abort ();

  if (__alignof__ (z128) < 128)
    abort (); 

  return 0;
}
