// Build don't link:
// Origin: Nathan Sidwell <nathan@codesourcery.com>
// Special g++ Options: -O2

struct A
{
  A (int) { }
  ~A () { }
  int get () const { return 0; }
};

void f (const A &s) {
  f (s.get ());
}
