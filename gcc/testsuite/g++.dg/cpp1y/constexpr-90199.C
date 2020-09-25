// PR c++/90199
// { dg-do compile { target c++14 } }
// { dg-additional-options "-frounding-math" }

template <typename>
class complex;

template <typename D7> constexpr complex<D7>
operator+ (complex<D7> hd, complex<D7> qc)
{
  hd += qc;
  return hd;
}

template <>
class complex<float> {
public:
  constexpr complex
  operator+= (complex rm)
  {
    jp += rm.jp;
    return *this;
  }

  _Complex float jp;
};

constexpr complex<float> fl{3.3}, ka{1.1}, r0 = fl + ka;
