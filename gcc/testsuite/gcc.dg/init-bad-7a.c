/* PR c/37724 */
/* { dg-do compile } */
/* { dg-options "" } */

struct f
{
  int *a;
};

char b[10];
struct f g = {b}; /* { dg-error "initialization of 'int \\*' from incompatible pointer type" } */
/* { dg-note "near initialization for" "" { target *-*-* } .-1 } */
