// PR c++/44628

template <typename T>
class Temp
{
  int Val;
  public:
  operator T&(void)  { return Val; }

  virtual T& operator=(T a ) // { dg-error "overriding" }
  {
    Val = a;
    return Val;
  }
};

class Int : public Temp<int>
{
  public:
  Int& operator=(int a) // { dg-error "conflicting return type" }
  {
    return (*this);
  }
};
