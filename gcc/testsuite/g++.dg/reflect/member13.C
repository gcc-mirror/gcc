// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int a;
  static int b;
  void foo () {}
  static void bar () {}
  template <int N>
  void baz () {}
  template <int N>
  static void qux () {}
};
int S::b = 42;

struct V {
  int a;
  static int b;
  void foo () {}
  static void bar () {}
  template <int N>
  void baz () {}
  template <int N>
  static void qux () {}
};
int V::b = 42;
static constexpr auto va = ^^V::a;
static constexpr auto vb = ^^V::b;
static constexpr auto vfoo = ^^V::foo;
static constexpr auto vbar = ^^V::bar;

void
corge ()
{
  S s {};
  s.[:^^V::a:]++;			// { dg-error ".V. is not a base of .S." }
  s.[:^^V::b:]++;			// { dg-error ".V. is not a base of .S." }
  s.[:^^V::foo:] ();			// { dg-error ".V::foo\\\(\\\). is not a member of .S." }
  s.[:^^V::bar:] ();			// { dg-error ".V::bar\\\(\\\). is not a member of .S." }
  s.[:va:]++;				// { dg-error ".V. is not a base of .S." }
  s.[:vb:]++;				// { dg-error ".V. is not a base of .S." }
  s.[:vfoo:] ();			// { dg-error ".V::foo\\\(\\\). is not a member of .S." }
  s.[:vbar:] ();			// { dg-error ".V::bar\\\(\\\). is not a member of .S." }
  s.template [:^^V::baz:] <42> ();	// { dg-error "is not a member of .S." }
  s.template [:^^V::qux:] <42> ();	// { dg-error "is not a member of .S." }
}
