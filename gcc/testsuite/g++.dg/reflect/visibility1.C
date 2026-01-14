// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -O0" }

#include <meta>

template <int N, std::meta::info I>
void
foo ()
{
}

struct A {};

namespace
{
  void baz (int) {}
  struct B {};
  struct C : public A {};
  template <typename T>
  struct G {};
}

struct D : public B {};

template <typename T>
struct F {};

struct H : public A {};

static union { int au; };
int b;
static int c;

constexpr auto ctx = std::meta::access_context::current ();

static void
bar (int x)
{
  int v = 42;
  foo <100, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi100EL" } } - var in TU-local fn
  foo <101, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi101EL" } } - var in TU-local fn
  foo <102, parameters_of (^^baz)[0]> ();	// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi102EL" } } - param of TU-local fn
  foo <103, bases_of (^^C, ctx)[0]> ();		// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi103EL" } } - direct base relationship with TU-local derived
  foo <104, bases_of (^^D, ctx)[0]> ();		// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi104EL" } } - direct base relationship with TU-local base
  foo <105, data_member_spec (^^B, { .name = "foo" })> (); // { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi105EL" } } - data member spec with TU-local type
}

inline void
qux (int x)
{
  int v = 42;
  foo <106, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi106EL" } } - var in inline fn - TODO, shall this be exported?
  foo <107, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi107EL" } } - var in inline fn - TODO, shall this be exported?
}

template <int N>
void
plugh (int x)
{
  int v = 42;
  foo <132, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi132EL" } } - var in public fn template instantiation - TODO, shall this be exported?
  foo <133, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi133EL" } } - var in public fn template instantiation - TODO, shall this be exported?
  foo <134, parameters_of (parent_of (^^v))[0]> (); // { dg-final { scan-assembler "\t.weak\t_Z3fooILi134EL" { target *-*-linux* } } } - fn parm of public fn template instantiation
}

namespace {
template <int N>
void
garply (int x)
{
  int v = 42;
  foo <135, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi135EL" } } - var in TU-local fn template instantiation
  foo <136, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi136EL" } } - var in Tu-local fn template instantiation
  foo <137, parameters_of (parent_of (^^v))[0]> (); // { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi137EL" } } - fn parm of TU-local fn template instantiation
}
}

template <typename T>
void
fred (int x)
{
  int v = 42;
  foo <138, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi138EL" } } - var in TU-local fn template instantiation
  foo <139, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi139EL" } } - var in Tu-local fn template instantiation
  foo <140, parameters_of (parent_of (^^v))[0]> (); // { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi140EL" { xfail *-*-* } } } - fn parm of TU-local fn template instantiation - TODO, I think this shouldn't be exported and the mangling of these 3 doesn't include the template parameter
}

[[=1]] void
xyzzy (int x)
{
  int v;
  struct E {};
  qux (x);
  foo <108, annotations_of (^^xyzzy)[0]> ();	// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi108EL" } } - annotations always TU-local
  foo <109, parameters_of (^^bar)[0]> ();	// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi109EL" } } - fn parm of TU-local fn
  foo <110, parameters_of (^^qux)[0]> ();	// { dg-final { scan-assembler "\t.weak\t_Z3fooILi110EL" { target *-*-linux* } } } - fn parm of inline fn
  foo <111, parameters_of (^^xyzzy)[0]> ();	// { dg-final { scan-assembler "\t.weak\t_Z3fooILi111EL" { target *-*-linux* } } } - fn parm of public fn
  foo <112, ^^B> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi112EL" } } - TU-local class
  foo <113, ^^C> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi113EL" } } - TU-local class
  foo <114, ^^D> ();				// { dg-final { scan-assembler "\t.weak\t_Z3fooILi114EL" { target *-*-linux* } } } - public class
  foo <115, ^^E> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi115EL" } } - TU-local class
  foo <116, ^^au> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi116EL" } } - anon union var at namespace scope
  foo <117, ^^F> ();				// { dg-final { scan-assembler "\t.weak\t_Z3fooILi117EL" { target *-*-linux* } } } - public template
  foo <118, ^^F <int>> ();			// { dg-final { scan-assembler "\t.weak\t_Z3fooILi118EL" { target *-*-linux* } } } - specialization of public template
  foo <119, ^^F <A>> ();			// { dg-final { scan-assembler "\t.weak\t_Z3fooILi119EL" { target *-*-linux* } } } - specialization of public template
  foo <120, ^^F <B>> ();			// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi120EL" } } - specialization with TU-local parameter
  foo <121, ^^G> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi121EL" } } - TU-local template
  foo <122, ^^G <int>> ();			// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi122EL" } } - specialization of TU-local template
  foo <123, ^^G <A>> ();			// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi123EL" } } - specialization of TU-local template
  foo <124, ^^G <B>> ();			// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi124EL" } } - specialization of TU-local template
  foo <125, ^^x> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi125EL" } } - var in public fn but non-comdat - TODO, shall this be exported?
  foo <126, ^^v> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi126EL" } } - var in public fn but non-comdat - TODO, shall this be exported?
  foo <127, std::meta::info {}> ();		// { dg-final { scan-assembler "\t.weak\t_Z3fooILi127EL" { target *-*-linux* } } } - null reflection
  foo <128, ^^b> ();				// { dg-final { scan-assembler "\t.weak\t_Z3fooILi128EL" { target *-*-linux* } } } - public variable
  foo <129, ^^c> ();				// { dg-final { scan-assembler-not "\t.weak\t_Z3fooILi129EL" } } - TU-local variable
  foo <130, data_member_spec (^^D, { .name = "foo" })> (); // { dg-final { scan-assembler "\t.weak\t_Z3fooILi130EL" { target *-*-linux* } } } - data member spec with public type
  foo <131, bases_of (^^H, ctx)[0]> ();		// { dg-final { scan-assembler "\t.weak\t_Z3fooILi131EL" { target *-*-linux* } } } - direct base relationship with both types public
  plugh <42> (x);
  garply <42> (x);
  fred <B> (x);
}
