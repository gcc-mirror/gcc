// PR c++/118390
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename T, int N>
constexpr int
foo (const T (&x)[N])
{
  return N;
}

static_assert (foo ({
  #embed __FILE__ limit (64)
}) == 64, "");
