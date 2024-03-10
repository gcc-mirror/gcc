/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16
foo (_Float16 x)/* { dg-error "SSE register return with SSE2 disabled" } */ 
{
  return x;/* { dg-error "SSE register return with SSE2 disabled" "" { target ia32 } } */
}
