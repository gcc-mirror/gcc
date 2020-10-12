// { dg-lto-do assemble }
// { dg-lto-options { { -flto -ffat-lto-objects -g } } }
struct A { A (int); };
template <class T> class B { T f; };
unsigned char *foo (int *, bool *, const int &);
template <typename, unsigned char *F (int *, bool *, const int &)> struct C {};
struct D { B<C<unsigned char, foo> > d; };
struct E { D e; };
struct F {};
struct G { static int bar (A, F, E, int); };

void
baz ()
{
  F f;
  G::bar (0, f, E (), 0);
}
