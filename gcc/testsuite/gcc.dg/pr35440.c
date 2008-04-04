/* PR c/35440 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

struct A {};
struct B { int i; char j[2]; };

void foo (void)
{
  (struct A){}();			/* { dg-error "called object" } */
  (struct B){ .i = 2, .j[1] = 1 }();	/* { dg-error "called object" } */
}
