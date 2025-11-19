// PR c++/122658
// { dg-do compile { target c++20 } }

struct S {
  consteval S () noexcept { }
  consteval S (const S &) = default;
};

template <typename T>
S
foo ()
{
  constexpr auto s = S();
  return s;
}

S
bar ()
{
  return foo <int> ();
}
