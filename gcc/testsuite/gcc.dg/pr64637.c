/* PR c/64637 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void g ();

void
f (int b)
{
  for (int i = 0; i < b; i + b) /* { dg-warning "28:statement with no effect" } */
    g ();
  // PARM_DECLs still don't have a location, don't expect an exact location.
  for (int i = 0; i < b; b) /* { dg-warning "statement with no effect" } */
    g ();
  for (int i = 0; i < b; !i) /* { dg-warning "26:statement with no effect" } */
    g ();
  for (!b;;) /* { dg-warning "8:statement with no effect" } */
    g ();
  for (;; b * 2) /* { dg-warning "13:statement with no effect" } */
    g ();
  ({
     b / 5; /* { dg-warning "8:statement with no effect" } */
     b ^ 5;
   });
}
