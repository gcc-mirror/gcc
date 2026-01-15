// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void foo (int);
template <typename T>
void bar (T);
struct S {
  void foo (int);
  template <typename T>
  void bar (T);
};

void
baz (S &s)
{
  template [: ^^foo :] (0);			// { dg-error "reflection 'foo' not usable in a template splice" }
  template [: ^^bar :] (0);			// { dg-message "only function templates are allowed here" "" { target *-*-* } .-1 }
  s.template [: ^^S::foo :] (0);		// { dg-error "reflection 'S::foo' not usable in a template splice" }
  s.template [: ^^S::bar :] (0);		// { dg-message "only function templates are allowed here" "" { target *-*-* } .-1 }
}

template <int N>
void
qux (S &s)
{
  // TODO: We don't reject this one.
  template [: N == 0 ? ^^foo : ^^:: :] (0);	// { dg-error "reflection 'foo' not usable in a template splice" "" { xfail *-*-* } }
  template [: N == 0 ? ^^bar : ^^:: :] (0);	// { dg-message "only function templates are allowed here" "" { xfail *-*-* } .-1 }
  s.template [: N == 0 ? ^^S::foo : ^^:: :] (0); // { dg-error "reflection 'foo' not usable in a template splice" "" { xfail *-*-* } }
  s.template [: N == 0 ? ^^S::bar : ^^:: :] (0);
}

void
corge (S &s)
{
  qux <0> (s);
}
