/* PR c/101312 */
/* { dg-do compile } */
/* { dg-options "-g" } */

volatile int a[1] __attribute__((may_alias));
