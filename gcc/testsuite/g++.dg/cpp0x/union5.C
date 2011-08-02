// PR c++/49803
// { dg-options -std=c++0x }

struct X
{
  X() = delete;
};

union Y
{
  // N3291=11-0061 12.6.2/8 says no initialization of
  // of other variant members (i.e. m_x) should
  // be performed.
  Y() : m_char1{ }
  { }

  struct
  {
    char m_char1;
  };

  X    m_x;
};
