// PR c++/119045
// { dg-do compile { target c++14 } }

constexpr float
foo ()
{
  __complex__ float f {1, 2};
  float s = __real__ f + __imag__ f;
  float &r = __real__ f;
  float &i = __imag__ f;
  r = 42;
  s += __real__ f;
  i = 3;
  s += __imag__ f;
  return s;
}

static_assert (foo () == 48.0f, "");
