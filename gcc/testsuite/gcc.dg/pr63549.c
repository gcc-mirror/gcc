/* PR c/63549 */
/* { dg-do compile } */
/* { dg-options "" } */

enum E e;
int a[10];
int i = a[e]; /* { dg-error "has an incomplete type" } */
