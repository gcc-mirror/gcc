namespace std {
  class ostream;
}

namespace N2 {
  class C0 {};
}

std::ostream& operator<<( std::ostream& os_, const N2::C0& m_);

namespace N1 {
  class C1 {
    friend std::ostream& operator<<(std::ostream& os, const C1& what);
  };

  class C2 {
    friend std::ostream& operator<<(std::ostream& os, const C2& what);
  };

  void foo(std::ostream & os, const N2::C0& m)
  {
    os << m; // Is this line valid?
  }
}
