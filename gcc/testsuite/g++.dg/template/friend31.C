// { dg-do compile }

// Origin: Ivan Godard <igodard@pacbell.net>

// PR c++/15410: Declaration of friend class template with wrong
// template parameter.

template <typename T, typename U> struct F; // { dg-error "previous declaration" }

class W
{
  template<int i> friend class F;	// { dg-error "template parameter" }
  int x;
};

template <typename T, typename U> struct F
{
  void Look(W& w) { w.x = 3; }
};

int main()
{
  W w;
  F<char, bool> f;
  f.Look(w);
  return 0;
}
