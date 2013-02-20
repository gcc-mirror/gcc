// PR c++/56343
// { dg-do compile { target c++11 } }

class B
{
public:
  virtual ~B() noexcept(false) { }
};

class D : public B
{
public:
  virtual ~D() = default;
};
