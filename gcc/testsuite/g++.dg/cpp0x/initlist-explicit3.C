// PR c++/109247
// { dg-do compile { target c++11 } }

template <typename _Tp> struct optional {
  template <typename _Up> explicit optional(_Up);
  template <typename _Up = _Tp> void operator=(_Up);
};
int setPattern_pattern;
struct SourceBrush {
  struct Brush {
    int brush;
  };
  void setPattern() { m_brush = {setPattern_pattern}; }
  optional<Brush> m_brush;
};
