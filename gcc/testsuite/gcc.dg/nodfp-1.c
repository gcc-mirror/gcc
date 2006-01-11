/* { dg-do compile { target {! dfp} } } */
/* { dg-options "-std=gnu99" } */

_Decimal32 x;	/* { dg-error "not supported" "reject decimal float" } */
_Decimal64 y;	/* { dg-error "not supported" "reject decimal float" } */
_Decimal128 z;	/* { dg-error "not supported" "reject decimal float" } */
