// PR c++/20679

template <class T>
struct foo
{
  struct bar
  {
    int m;
  };

  void m() const {}
  void m() {}
  
  bool n() const { return b->m < 42; }

  bar *b;
};



