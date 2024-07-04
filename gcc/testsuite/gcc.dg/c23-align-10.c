/* Test C2Y alignof on an incomplete array type: not allowed in C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a = alignof(int[]); /* { dg-error "incomplete" } */
int b = alignof(int[][1]); /* { dg-error "incomplete" } */
