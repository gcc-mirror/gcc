// { dg-do compile }
// { dg-options "-O3" }

struct B { virtual unsigned f () const; };
struct C { virtual void f (); };
struct F { virtual unsigned f (bool) const; ~F (); };
struct J : C, F {};
struct G : J { unsigned f (bool) const { return 0; } };
struct H : B
{
  H (int);
  unsigned f () const { return ((const F &) h).f (0); }
  G h;
};
H h (0);
