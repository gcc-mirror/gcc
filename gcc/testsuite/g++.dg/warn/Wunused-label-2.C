// { dg-do compile }
// { dg-options "-Wunused" }

// If __attribute__ ((unused)) follows a label and precedes a
// declaration, we should get a warning for the label, not the
// declaration.

void
f1()
{
  int i1;				// { dg-warning "unused variable" }
 l1: __attribute__ ((unused)) int i2;	// { dg-warning "label \[^\n\]* not used" }
}
