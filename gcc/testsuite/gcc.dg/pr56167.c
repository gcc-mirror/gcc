/* PR middle-end/56167 */
/* { dg-do compile } */

extern void foo (void) __attribute__ ((error (0)));	/* { dg-warning "attribute ignored" } */
extern void bar (void) __attribute__ ((warning (0)));	/* { dg-warning "attribute ignored" } */
int var __attribute__ ((error ("foo")));		/* { dg-warning "attribute ignored" } */

int
main ()
{
  foo ();
  bar ();
  var++;
  return 0;
}
