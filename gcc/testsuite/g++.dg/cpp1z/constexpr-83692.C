// PR c++/83692
// { dg-do compile { target c++17 } }

struct integer {
  constexpr int value() const { return m_value;	}
  int m_value;
};

struct outer {
  integer m_x{0};
  constexpr outer()
    {
      if (m_x.value() != 0)
	throw 0;
      m_x.m_value = integer{1}.value();
      if (m_x.value() != 1)
	throw 0;
    }
};

constexpr outer o{};
