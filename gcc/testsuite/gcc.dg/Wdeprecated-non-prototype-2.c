/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wc11-c23-compat" } */

void f1 ();
void f2 (); /* { dg-note "declared here" } */

void
g ()
{
  f1 ();
  f2 (1); /* { dg-warning "does not allow arguments for function" } */
}

void
f1 ()
{
}

void
f2 (int i) /* { dg-warning "does not allow defining parameters for function" } */
{
}
