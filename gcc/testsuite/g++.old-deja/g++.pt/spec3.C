extern "C" void abort();

class X
{
  public:
    virtual int f() const = 0;
};

template <class T>
class Y: public X
{
  public:
    virtual int f() const;
};

template <class T>
int Y<T>::f() const
{
  abort();
  return 0;
}

template <>
int Y<bool>::f() const;

template <>
int Y<bool>::f() const
{
  return 0;
}

int main()
{
  Y<bool> yb;

  yb.f();
}
