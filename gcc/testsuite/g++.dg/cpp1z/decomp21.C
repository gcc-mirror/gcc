// { dg-options -std=c++1z }

int a[3];
struct S { int b, c, d; } s;
void
foo ()
{
  auto [ b, c, d ] = a;
  auto [ e, f, g ] = s;
  auto [ h, i, j ] { s };
  auto [ k, l, m ] { s, };
  auto [ n, o, p ] { a };
  auto [ q, r, t ] ( s );
  auto [ u, v, w ] ( s, );      // { dg-error "expected primary-expression before '.' token" }
  auto [ x, y, z ] ( a );       // { dg-error "expression list treated as compound expression in initializer" "" { target *-*-* } .-1 }
}
