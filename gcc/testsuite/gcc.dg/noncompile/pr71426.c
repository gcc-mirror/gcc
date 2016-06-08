/* PR c/71426 */
/* { dg-do compile } */
/* { dg-options "-w" } */

int f (int x[x - x ()]); /* { dg-error "undeclared" } */
