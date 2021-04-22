// PR c++/16617

class B
{
  protected:
  int i;
};

template <class T> void fr ();

class D2 : public B
{
  friend void fr<int> ();
};

template<int B::*> struct X
{};

template <class T> void fr ()
{
  X<&B::i> x1;  // { dg-error "protected" }
  X<&D2::i> x2; // { dg-error "protected" }
}

template void fr<char>();
