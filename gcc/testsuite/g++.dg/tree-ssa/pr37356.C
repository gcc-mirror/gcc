// PR middle-end/37356 */
// { dg-do compile }
// { dg-options "-O" }

bool foo ();
int bar ();

bool
baz (int v)
{
  return v == bar ();
}

struct A
{
  A () { baz (1) || foo (); }
};

struct B
{
  static A get () { return A (); }
  B (const int &x) { }
  B () : b (get ()) { }
  A b;
};

B c;

void
test ()
{
  int d;
  c = d;
}
