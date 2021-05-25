// PR c++/100666
// { dg-do compile { target c++11 } }

[[nodiscard]] decltype(nullptr) bar ();
extern void foo (...);
template <typename T> void qux (T);

void
baz ()
{
  foo (bar ());		// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  bar ();		// { dg-warning "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  auto x = bar ();	// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  qux (bar ());		// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
}
