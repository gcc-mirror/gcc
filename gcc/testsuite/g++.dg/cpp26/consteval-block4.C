// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.
// Test that we actually evaluate the consteval block.

void bar () { }

template<int N>
constexpr void
fn ()
{
  if (N > 0)
    bar ();
}

template<int N>
struct S {
  consteval { fn<N>(); }
};

S<0> s;

template<int N>
constexpr void
fn2 ()
{
  if (N > 0)
    bar ();
}

template<int N>
void
g ()
{
  consteval { fn2<N>(); }
}

void
f ()
{
  g<0>();
}
