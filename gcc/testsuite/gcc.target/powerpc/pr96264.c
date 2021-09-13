/* { dg-do run { target { powerpc64le-*-* } } } */
/* { dg-options "-Os -fno-forward-propagate -fschedule-insns -fno-tree-ter -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

typedef unsigned char __attribute__ ((__vector_size__ (64))) v512u8;
typedef unsigned short u16;
typedef unsigned short __attribute__ ((__vector_size__ (64))) v512u16;
typedef unsigned __int128 __attribute__ ((__vector_size__ (64))) v512u128;

v512u16 d;
v512u128 f;

v512u8
foo (u16 e)
{
  v512u128 g = f - -e;
  d = (5 / (d + 1)) < e;
  return (v512u8) g;
}

int
main (void)
{
  v512u8 x = foo (2);
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != (i % 16 ? 0 : 2))
      __builtin_abort ();
}
