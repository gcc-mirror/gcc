/* PR c++/82466 */
/* { dg-options "-Wbuiltin-declaration-mismatch" } */

int printf;  /* { dg-warning "declared as non-function" } */
