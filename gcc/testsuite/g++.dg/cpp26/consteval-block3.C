// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.
// Test that we actually evaluate the consteval block.

void bar () { }

template<int N>
constexpr void
fn ()
{
  if (N > 0)
    bar ();			// { dg-error "call to non-.constexpr. function" }
}

template<int N>
struct S {
  consteval { fn<N>(); }	// { dg-error "called in a constant expression" }
};

S<1> s;

template<int N>
constexpr void
fn2 ()
{
  if (N > 0)
    bar ();			// { dg-error "call to non-.constexpr. function" }
}

template<int N>
void
g ()
{
  consteval { fn2<N>(); }	// { dg-error "called in a constant expression" }
}

void
f ()
{
  g<1>();
}
