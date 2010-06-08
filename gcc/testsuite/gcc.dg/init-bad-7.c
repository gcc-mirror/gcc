/* PR c/37724 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

struct f
{
  int *a;
};

char b[10];
struct f g = {b}; /* { dg-warning "initialization from incompatible pointer type|near initialization for" } */
