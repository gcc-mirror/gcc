/* PR c/60139 */
/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */
/* { dg-prune-output ".*near initialization for.*" } */

double sin (double);
void
fn (int *p)
{
  int **a[] = { &p, /* { dg-warning "17:initializer element is not computable at load time" } */
               (void *) 0, &p }; /* { dg-warning "28:initializer element is not computable at load time" } */
  double d[] = { sin (1.0), /* { dg-warning "18:initializer element is not a constant expression" } */
                 8.8, sin (1.0), 2.6 }; /* { dg-warning "23:initializer element is not a constant expression" } */
}
