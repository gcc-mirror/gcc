// Make sure C99 complex works with constexpr
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct complex
{
  typedef float value_type;
  typedef __complex__ float _ComplexT;

  constexpr complex(_ComplexT __z) : _M_value(__z) { }

  constexpr complex(float __r = 0.0f, float __i = 0.0f)
  : _M_value(__r + __i * 1.0fi) { }

private:
  _ComplexT _M_value;
};
constexpr complex c1;
