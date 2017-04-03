// PR target/80102
// { dg-do compile }
// { dg-options "-fnon-call-exceptions -Os" }
// { dg-additional-options "-mminimal-toc" { target { powerpc*-*-* && lp64 } } }

struct B { float a; B (float c) { for (int g; g < c;) ++a; } };
struct D { D (B); };

int
main ()
{
  B (1.0);
  D e (0.0), f (1.0);
}
