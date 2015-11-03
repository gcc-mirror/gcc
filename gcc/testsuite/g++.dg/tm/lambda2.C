// Test for lambda call.
// { dg-options "-fgnu-tm -std=c++14" }

void unsafe ();
void f() transaction_safe
{
  []{}(); 			// OK, implicitly transaction-safe.
  []{unsafe();}();		// { dg-error "unsafe" }
}
