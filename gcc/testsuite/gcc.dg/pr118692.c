/* { dg-do compile } */
/* { dg-options "-O2" } */

_Complex double cf;

void
foo(char c)
{
  cf += *(_Complex double *)__builtin_memcpy(8143523003042804629LL + &c, 0, 0);
}
