// PR c++/34965
// { dg-do compile }
// { dg-options "-O" }

int foo (int);

void
bar (int i, int j, double k)
{
  foo (i && j) ();	// { dg-error "\\(\\(?i != 0\\)? \\&\\& \\(?j != 0\\)?\\)" }
  foo (!i || !j) ();	// { dg-error "function" }
  foo (!i == !j) ();	// { dg-error "function" }
}
