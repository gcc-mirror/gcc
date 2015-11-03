// Test for composite pointer type.
// { dg-options -fgnu-tm }

void f(bool b)
{
  void (*p)() transaction_safe = 0;
  void (*g)() = 0;

  g = b ? p : g;		// OK
  p = b ? p : g;		// { dg-error "" }

  p == g;
  p != g;
}
