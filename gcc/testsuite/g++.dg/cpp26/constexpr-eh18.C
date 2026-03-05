// PR c++/124145
// { dg-do compile { target c++26 } }

struct S {};

constexpr S
foo (S x)
{
  throw 123;
  return x;
}

constexpr void
bar ()
{
  foo (S {});
}

constexpr bool
baz ()
{
  try
    {
      bar ();
      asm  ("");
    }
  catch (int)
    {
    }

  try
    {
      bar ();
      asm ("");
    }
  catch (int)
    {
    }
  return true;
}

static_assert (baz ());
