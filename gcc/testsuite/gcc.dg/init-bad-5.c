/* PR c/28136 */
/* { dg-do compile } */
/* { dg-options "" } */

int i = (struct A[]) {};  /* { dg-error "incomplete|empty|initialization" } */
