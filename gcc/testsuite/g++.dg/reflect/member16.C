// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A { int a; };
struct B : public A { int b; };
struct C : public B { int c; };
struct D : public A, B, C { int d; };	// { dg-warning "direct base 'A' inaccessible in 'D' due to ambiguity" }
					// { dg-warning "direct base 'B' inaccessible in 'D' due to ambiguity" "" { target *-*-* } .-1 }
struct E { int _; };
struct F : public E { int _; };
struct G : public F { int _; };
struct H : public E, F, G { int _; };	// { dg-warning "direct base 'E' inaccessible in 'H' due to ambiguity" }
					// { dg-warning "direct base 'F' inaccessible in 'H' due to ambiguity" "" { target *-*-* } .-1 }
struct I { int i; };
struct J { int _; };

void
foo (C &c, D &d, G &g, H &h)
{
  c.[: ^^A::a :] = 1;
  c.[: ^^B::b :] = 2;
  c.[: ^^C::c :] = 3;
  d.[: ^^A::a :] = 4;			// { dg-error "'A' is an ambiguous base of 'D'" }
  d.[: ^^B::b :] = 5;			// { dg-error "'B' is an ambiguous base of 'D'" }
  d.[: ^^C::c :] = 6;
  d.[: ^^D::d :] = 7;
  d.[: ^^I::i :] = 8;			// { dg-error "'I' is not a base of 'D'" }
  g.[: ^^E::_ :] = 1;
  g.[: ^^F::_ :] = 2;
  g.[: ^^G::_ :] = 3;
  h.[: ^^E::_ :] = 4;			// { dg-error "'E' is an ambiguous base of 'H'" }
  h.[: ^^F::_ :] = 5;			// { dg-error "'F' is an ambiguous base of 'H'" }
  h.[: ^^G::_ :] = 6;
  h.[: ^^H::_ :] = 7;
  h.[: ^^J::_ :] = 8;			// { dg-error "'J' is not a base of 'H'" }
}

template <typename S, typename T, typename U, typename V>
void
bar (S &c, T &d, U &g, V &h)
{
  c.[: ^^A::a :] = 1;
  c.[: ^^B::b :] = 2;
  c.[: ^^C::c :] = 3;
  d.[: ^^A::a :] = 4;			// { dg-error "'A' is an ambiguous base of 'D'" }
  d.[: ^^B::b :] = 5;			// { dg-error "'B' is an ambiguous base of 'D'" }
  d.[: ^^C::c :] = 6;
  d.[: ^^D::d :] = 7;
  d.[: ^^I::i :] = 8;			// { dg-error "invalid use of non-static data member 'I::i'" }
  g.[: ^^E::_ :] = 1;
  g.[: ^^F::_ :] = 2;
  g.[: ^^G::_ :] = 3;
  h.[: ^^E::_ :] = 4;			// { dg-error "'E' is an ambiguous base of 'H'" }
  h.[: ^^F::_ :] = 5;			// { dg-error "'F' is an ambiguous base of 'H'" }
  h.[: ^^G::_ :] = 6;
  h.[: ^^H::_ :] = 7;
  h.[: ^^J::_ :] = 8;			// { dg-error "invalid use of non-static data member 'J::_'" }
}

void
baz (C &c, D &d, G &g, H &h)
{
  bar (c, d, g, h);
}

template <int N>
void
qux (C &c, D &d, G &g, H &h)
{
  c.[: ^^A::a :] = 1;
  c.[: ^^B::b :] = 2;
  c.[: ^^C::c :] = 3;
  d.[: ^^A::a :] = 4;			// { dg-error "'A' is an ambiguous base of 'D'" }
  d.[: ^^B::b :] = 5;			// { dg-error "'B' is an ambiguous base of 'D'" }
  d.[: ^^C::c :] = 6;
  d.[: ^^D::d :] = 7;
  d.[: ^^I::i :] = 8;			// { dg-error "'I' is not a base of 'D'" }
  g.[: ^^E::_ :] = 1;
  g.[: ^^F::_ :] = 2;
  g.[: ^^G::_ :] = 3;
  h.[: ^^E::_ :] = 4;			// { dg-error "'E' is an ambiguous base of 'H'" }
  h.[: ^^F::_ :] = 5;			// { dg-error "'F' is an ambiguous base of 'H'" }
  h.[: ^^G::_ :] = 6;
  h.[: ^^H::_ :] = 7;
  h.[: ^^J::_ :] = 8;			// { dg-error "'J' is not a base of 'H'" }
}
