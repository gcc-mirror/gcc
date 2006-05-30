/* PR c/27718 */
/* { dg-do compile } */

int i = sizeof(struct A[]);  /* { dg-error "incomplete" } */
