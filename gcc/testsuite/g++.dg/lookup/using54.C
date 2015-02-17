// PR c++/60894

struct B
{
  struct S {};
};

struct D : B
{
  using B::S;
  void doIt(struct S&);
};

void D::doIt(struct S&)
{
}
