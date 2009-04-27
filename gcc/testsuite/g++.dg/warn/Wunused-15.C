// PR c++/39875
// { dg-do compile }
// { dg-options "-Wunused-value" }

int *i;
void
foo ()
{
  *i++;		// { dg-warning "value computed is not used" }
  (void) *i++;	// { dg-bogus "value computed is not used" }
}
