// { dg-do compile }

// Origin: aroach@stoic.electriceyeball.com

// PR c++/3332: Friend function lookup in class defined outside its
// namespace

namespace N
{
  class A;
}

class N::A
{
  void x();
  friend void func(void);
};

namespace N
{
  void func(void);
}

void N::func(void)
{
  N::A a;
  a.x();
}

int main()
{
  return 0;
}

