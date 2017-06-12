/* PR c/79730 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

register int x() asm (""); /* { dg-error "invalid storage class" } */
register float y() asm (""); /* { dg-error "invalid storage class" } */
