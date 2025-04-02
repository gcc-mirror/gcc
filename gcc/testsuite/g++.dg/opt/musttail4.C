// { dg-do compile { target { external_musttail && c++11 } } }
// { dg-options "-O2 -fexceptions" }

struct S { ~S (); };
volatile int v;
struct T { ~T () { v = v + 1; } };
struct U { ~U () {} };
int foo ();

int
bar () noexcept
{
  [[gnu::musttail]] return foo ();	// { dg-error "cannot tail-call: call may throw exception that does not propagate" }
}

int
baz ()
{
  S s;
  [[gnu::musttail]] return foo ();	// { dg-error "cannot tail-call: other reasons" }
}

int
qux ()
{
  T t;
  [[gnu::musttail]] return foo ();	// { dg-error "cannot tail-call: other reasons" }
}

int
corge ()
{
  U u;
  [[gnu::musttail]] return foo ();
}
