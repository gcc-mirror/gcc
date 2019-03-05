// PR debug/85550
// { dg-do link }
// { dg-options "-O2 -g -fdebug-types-section" }

struct A {
  int bar () const { return 0; }
};
template <int (A::*foo)() const>
struct B {
};

B<&A::bar> b;

int
main ()
{
}
