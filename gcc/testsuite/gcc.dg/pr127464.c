/* PR c/127464 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wattributes" } */

[[gnu::callback_only (1, 2, 3), gnu::callback_only (4, 5)]]
void bar (); /* { dg-warning "requires a prototype" }*/

[[gnu::callback_only (1, 2, 3)]]
void foo (void (*) (), int *, double); /* { dg-warning "must have a prototype" } */
