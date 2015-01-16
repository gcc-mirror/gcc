// PR target/64338
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-mtune=nehalem -march=i586" { target { { i?86-*-* x86_64-*-* } && ia32 } } }

enum O {};
struct A { A (); };
struct B { int fn1 (); };
struct C { struct D; D *fn2 (); void fn3 (); int fn4 (); };
struct F { void fn5 (const int & = 0); };
struct G { F *fn6 (); };
struct H { int h; };
struct C::D { friend class C; G *fn7 (); };
O a;

void
C::fn3 ()
{
  int b = a;
  H c;
  if (b)
    fn2 ()->fn7 ()->fn6 ()->fn5 ();
  double d;
  if (fn4 ())
    d = c.h > 0;
  A e (b ? A () : A ());
  B f;
  f.fn1 () && d && fn2 ();
}
