// PR c++/11063
// { dg-options "" }

class Foo
{
private:
  const int val_[2];

public:
  Foo(int, int);
};

Foo::Foo(int v0, int v1)
  : val_((int[]) {v0, v1})  // { dg-error "" "" }
{
}
