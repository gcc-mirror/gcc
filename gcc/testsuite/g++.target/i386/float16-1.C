/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16	/* { dg-error "expected unqualified-id before '_Float16'" } */
foo (_Float16 x) 
{
  return x;
}		/* { dg-error "'_Float16' is not supported on this target" } */
