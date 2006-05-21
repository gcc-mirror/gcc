// PR c++/27210

template <class foo> class junk {
  void bar(int a)
  {
    unsigned char *c = new unsigned char[a*sizeof(foo)];
  }
};

