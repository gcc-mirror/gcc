// PR c++/43663

struct S
{
  S(): i(0) {}
  int i : 3;
};

S s;

const int& cr(s.i);
