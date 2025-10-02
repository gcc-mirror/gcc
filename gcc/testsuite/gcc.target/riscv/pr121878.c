/* { dg-do compile } */
/* { dg-additional-options "-mcpu=tt-ascalon-d8 -mtune=tt-ascalon-d8" } */

/* Verify we don't ICE on the following test case.  */

typedef int __attribute__((__vector_size__ (32))) vec;
vec
foo (vec x, vec y)
{
  return x / y;
}
