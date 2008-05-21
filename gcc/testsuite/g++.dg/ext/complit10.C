// PR c++/36023
// { dg-do compile }
// { dg-options "" }

struct A;

void
f1 (int i)
{
  (int[i]) { 1 };	// { dg-error "variable-sized compound literal" }
  (A[5]) { 1 };		// { dg-error "have incomplete type" }
  (A[i]) { 1 };		// { dg-error "have incomplete type" }
}

void
f2 ()
{
  (int[]) { 1 };
  (int[1]) { 1 };
}
