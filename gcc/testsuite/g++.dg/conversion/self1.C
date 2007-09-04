// PR c++/31419

struct B
{
  template<typename T>
  operator T const& () const
  {
    return 42;
  }
};

B f()
{
  return B();
}
