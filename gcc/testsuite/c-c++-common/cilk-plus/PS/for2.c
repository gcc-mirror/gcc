/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

#pragma simd		/* { dg-error "must be inside a function" } */

void foo()
{
}
