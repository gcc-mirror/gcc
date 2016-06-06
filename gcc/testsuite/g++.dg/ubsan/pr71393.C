// PR c++/71393
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

struct B { B &operator << (long); };
struct A { A (); long a, b, c, d, e, f; };

A::A ()
{
  B q;
  q << 0 << a << 0 << b << 0 << (b / a) << 0 << c << 0 << (c / a) << 0
    << d << 0 << (d / a) << 0 << e << 0 << (e / a) << 0 << f << 0
    << (f / a) << 0;
}
