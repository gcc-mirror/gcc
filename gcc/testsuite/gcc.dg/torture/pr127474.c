/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-additional-options "-march=z17" { target s390x-*-* } } */

/* Previously we ICE'd in expand_mult when dealing with a CONST_WIDE_INT.  */

typedef __int128 v1ti __attribute__ ((vector_size (16)));

v1ti foo (v1ti x)
{
  return x * (v1ti){(__int128)123456789 << 64 | (__int128)123456789};
}
