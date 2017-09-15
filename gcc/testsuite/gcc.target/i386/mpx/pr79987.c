/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

extern void foo;
void *bar = &foo; /* { dg-warning "taking address of expression of type .void." } */
