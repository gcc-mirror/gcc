// PR c++/53567
// { dg-do compile { target c++11 } }

template <unsigned int, bool> struct IntegerType { typedef unsigned type; };

template <class EnumT>
using UnderlyingEnumType = typename IntegerType<sizeof(EnumT), (EnumT(-1) > EnumT(0))>::type;

template <class EnumT, class UnderlyingT = UnderlyingEnumType<EnumT>>
struct EnumMask
{
  constexpr EnumMask(EnumT val) : m_val(val) {}
  operator EnumT() { return m_val; }

  EnumT m_val;
};

enum class A : unsigned { x };

template <class EnumT>
EnumMask<EnumT> operator ~(EnumT lhs)
{
  return EnumT(~unsigned(lhs) & unsigned(EnumT::maskAll)); // { dg-error "not a member" }

}

int main()
{
  ~A::x;
  return 0;
}
