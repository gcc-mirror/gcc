/* PR c/71426 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -w" } */

int f (int x[x - x ()]); /* { dg-error "undeclared" } */
