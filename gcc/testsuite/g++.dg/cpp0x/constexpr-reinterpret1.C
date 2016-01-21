// PR c++/56728
// { dg-require-effective-target c++11 }

class B {
public:
  static B instance;
  class Inner
  {
  public:
    class Wuzi
    {
      unsigned int m;
    } m_Class[3];
    unsigned m_Int[4];
  };

  constexpr static Inner & getInner()
  /* I am surprised this is considered a constexpr */
  { return *((Inner *)4); } // { dg-error "reinterpret_cast" "" }
};

B B::instance;

class A
{
public:
  constexpr A(B &bridge, B::Inner &bridge2, unsigned char index)
    : m_Bridge(bridge), m_Wuz(bridge2.m_Class[index])
  {}

  B &m_Bridge;
  B::Inner::Wuzi &m_Wuz;
};
A works{B::instance, B::getInner(), 3};
A crashnkill[1]{{B::instance, B::getInner(), 3}};
