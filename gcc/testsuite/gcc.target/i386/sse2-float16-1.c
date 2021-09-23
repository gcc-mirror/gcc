/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16/* { dg-error "is not supported on this target" } */
foo (_Float16 x) /* { dg-error "is not supported on this target" } */
{
  return x;
}
