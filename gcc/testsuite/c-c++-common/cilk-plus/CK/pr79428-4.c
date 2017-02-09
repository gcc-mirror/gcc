/* PR c/79428 */
/* { dg-options "-fcilkplus" } */
#pragma cilk grainsize /* { dg-error "must be inside a function" } */
