/* PR c/90658 */
/* { dg-do compile { target init_priority } } */

void f ();
void g1 () __attribute__ ((constructor(f))); /* { dg-error "priorities must be integers" } */
void g2 () __attribute__ ((destructor(f))); /* { dg-error "priorities must be integers" } */
