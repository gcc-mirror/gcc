/* PR c/79428 */
/* { dg-options "-fcilkplus" } */
#pragma simd /* { dg-error "must be inside a function" } */
