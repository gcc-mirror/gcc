// PR c++/65075
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }

template <typename> class C
{
  struct
  {
    int i;
  };
  auto operator*(const C m) -> decltype (m.i);
};
void fn1 (const C<float>);
C<float> a;
