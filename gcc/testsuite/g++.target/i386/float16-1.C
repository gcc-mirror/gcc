/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16/* { dg-error "does not name a type" } */
foo (_Float16 x) 
{
  return x;
}
