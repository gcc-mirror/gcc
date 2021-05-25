// PR c++/100666
// { dg-do compile { target c++11 } }

struct S {};
[[nodiscard]] S bar ();
struct U { S s; };
[[nodiscard]] U corge ();
extern void foo (...);
template <typename T> void qux (T);

void
baz ()
{
  foo (bar ());		// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  bar ();		// { dg-warning "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  auto x = bar ();	// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  qux (bar ());		// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  foo (corge ());	// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  corge ();		// { dg-warning "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  auto y = corge ();	// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
  qux (corge ());	// { dg-bogus "ignoring return value of '\[^\n\r]*', declared with attribute 'nodiscard'" }
}
