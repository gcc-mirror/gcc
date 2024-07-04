/* Test C2Y alignof on an incomplete array type.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

int a = alignof(int[]);
int b = alignof(int[][1]);
