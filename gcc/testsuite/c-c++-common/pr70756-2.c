/* PR c/70756 */
/* { dg-do compile } */
/* { dg-options "-Wpointer-arith" } */

extern void bar (void);

void
fn (void *p)
{
  void *a = p + 1; /* { dg-warning "15:pointer of type" } */
  void (*a2)(void) = &bar + 1; /* { dg-warning "27:pointer to a function" } */
}
